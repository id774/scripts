#!/bin/sh

########################################################################
# vmware-rebuild-and-sign.sh: Rebuild & sign VMware kernel modules
#
#  Description:
#  This script rebuilds VMware Workstation modules (vmmon/vmnet) for the
#  current running kernel and signs them with a locally enrolled MOK key
#  when they are unsigned. It is idempotent: already-signed modules are
#  skipped. After signing, it runs depmod and attempts to modprobe both
#  modules. Designed for Secure Boot environments on Debian-family systems.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./vmware-rebuild-and-sign.sh
#  Run as a regular user with sudo privileges; the script will invoke sudo
#  where necessary.
#
#  Notes:
#  - No backups are created; operations are safe and idempotent.
#  - If vmware-modconfig fails, the script continues to signing checks so
#    you can re-run after fixing toolchain issues.
#  - Requires an enrolled MOK (Machine Owner Key) in UEFI for Secure Boot.
#
#  Requirements:
#  - Linux operating system (Debian/Ubuntu family recommended)
#  - sudo privileges for building/signing kernel modules
#  - Commands: sudo, modinfo, depmod, vmware-modconfig, awk, grep, sh
#
#  Version History:
#  v1.0 2025-09-07
#       Initial release.
#
########################################################################

KVER="$(uname -r)"
MODDIR="/lib/modules/$KVER/misc"
KEY="/etc/vmware/module-signing/MOK.key"
CRT="/etc/vmware/module-signing/MOK.crt"
SIGN_FILE_PRIMARY="/usr/src/linux-headers-$KVER/scripts/sign-file"

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the system is Linux
check_system() {
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if required commands are available and executable
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Resolve sign-file path for this kernel
find_sign_file() {
    if sudo test -x "$SIGN_FILE_PRIMARY"; then
        echo "$SIGN_FILE_PRIMARY"
        return 0
    fi
    # Fallback to linux-kbuild-* provided path
    for d in /usr/lib/linux-kbuild-*; do
        if sudo test -x "$d/scripts/sign-file"; then
            echo "$d/scripts/sign-file"
            return 0
        fi
    done
    echo ""
    return 1
}

# Predicate: module file exists
has_module() {
    sudo test -f "$1"
}

# Predicate: module is already signed (modinfo signer is non-empty)
is_signed() {
    signer="$(modinfo -F signer "$1" 2>/dev/null || true)"
    [ -n "$signer" ]
}

# Rebuild VMware modules for the running kernel (non-fatal on failure)
rebuild_modules() {
    echo "[INFO] Rebuilding VMware modules for kernel $KVER (vmware-modconfig)"
    if ! sudo vmware-modconfig --console --install-all; then
        echo "[WARN] vmware-modconfig failed; continuing to signing checks." >&2
    fi
}

# Sign a single module if it is currently unsigned
sign_module_if_needed() {
    modpath="$1"
    if ! has_module "$modpath"; then
        echo "[WARN] Module not found: $modpath (skipping)"
        return 0
    fi
    if is_signed "$modpath"; then
        signer="$(modinfo -F signer "$modpath" 2>/dev/null || true)"
        echo "[INFO] Already signed: $modpath (signer: ${signer:-unknown})"
        return 0
    fi
    SF="$(find_sign_file)"
    if [ -z "$SF" ]; then
        echo "[ERROR] sign-file script not found for kernel $KVER." >&2
        return 1
    fi
    if ! sudo test -r "$KEY"; then
        echo "[ERROR] Missing private key: $KEY" >&2
        return 1
    fi
    if ! sudo test -r "$CRT"; then
        echo "[ERROR] Missing certificate: $CRT" >&2
        return 1
    fi
    echo "[INFO] Signing $modpath"
    if ! sudo "$SF" sha256 "$KEY" "$CRT" "$modpath"; then
        echo "[ERROR] Signing failed for $modpath" >&2
        return 1
    fi
    return 0
}

# Attempt to load modules (non-fatal; useful on Secure Boot to verify)
try_load_modules() {
    if ! lsmod | grep -q '^vmmon'; then
        sudo modprobe vmmon 2>/dev/null || echo "[WARN] modprobe vmmon failed."
    fi
    if ! lsmod | grep -q '^vmnet'; then
        sudo modprobe vmnet 2>/dev/null || echo "[WARN] modprobe vmnet failed."
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands modinfo depmod awk grep vmware-modconfig
    check_sudo

    # Ensure module directory exists (created by vmware-modconfig when it succeeds)
    rebuild_modules
    if ! sudo test -d "$MODDIR"; then
        echo "[WARN] $MODDIR not found yet; continuing (re-run after fixing build if needed)." >&2
    fi

    # Sign modules if needed
    sign_module_if_needed "$MODDIR/vmmon.ko" || exit 1
    sign_module_if_needed "$MODDIR/vmnet.ko" || exit 1

    # Refresh module dependency map for this kernel
    echo "[INFO] Running depmod for $KVER"
    if ! sudo depmod -a "$KVER"; then
        echo "[WARN] depmod failed for $KVER" >&2
    fi

    # Try to load modules so the host becomes ready immediately
    try_load_modules

    echo "[INFO] Completed: vmmon/vmnet modules processed for kernel $KVER. All signing and depmod steps finished successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

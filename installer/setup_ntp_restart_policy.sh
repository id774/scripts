#!/bin/sh

########################################################################
# setup_ntp_restart_policy.sh: Deploy Restart=on-failure for NTP services
#
#  Description:
#  Deploy a systemd drop-in that sets Restart=on-failure and RestartSec=30
#  for available NTP services on this host. Targets are:
#    - ntpsec.service
#    - ntp.service
#    - myntp.service
#
#  Drop-in path:
#      /etc/systemd/system/<service>.service.d/restart.conf
#
#  Drop-in content:
#      [Service]
#      Restart=on-failure
#      RestartSec=30
#
#  Behavior:
#    - Idempotent deployment: only writes when missing or different.
#    - Detects existing target units (enabled/disabled does not matter).
#    - Runs `systemctl daemon-reload` only when changes are made.
#    - Does not restart services automatically.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./setup_ntp_restart_policy.sh
#
#  Notes:
#  - This script requires systemd; exits if systemctl is not available.
#  - No backups are created; deployment is applied directly when needed.
#
#  Requirements:
#  - Linux operating system
#  - sudo privileges to write under /etc/systemd/system and reload systemd
#  - Commands: sudo, awk, grep, cmp, mktemp, mkdir, mv, rm, chmod, chown,
#              uname, systemctl, test
#
#  Version History:
#  v1.0 2025-10-21
#       Initial release.
#
########################################################################

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

# Detect existing target units (enabled/disabled does not matter)
detect_services() {
    SERVICES_FOUND=""
    for s in ntpsec ntp myntp; do
        if systemctl list-unit-files --type=service --no-legend --no-pager 2>/dev/null | awk '{print $1}' | grep -qx "${s}.service"; then
            SERVICES_FOUND="$SERVICES_FOUND $s"
            continue
        fi
        if [ -f "/lib/systemd/system/${s}.service" ] || [ -f "/etc/systemd/system/${s}.service" ]; then
            SERVICES_FOUND="$SERVICES_FOUND $s"
        fi
    done
}

# Write drop-in file if absent or different (idempotent)
deploy_dropin_for() {
    svc="$1"
    dir="/etc/systemd/system/${svc}.service.d"
    file="${dir}/restart.conf"

    body="[Service]
Restart=on-failure
RestartSec=30
"

    if ! sudo test -d "$dir"; then
        if ! sudo mkdir -p "$dir"; then
            echo "[ERROR] Failed to create directory: $dir" >&2
            exit 1
        fi
    fi

    tmp="$(mktemp /tmp/setup_ntp_restart_policy.XXXXXX)" || {
        echo "[ERROR] Failed to create temp file." >&2
        exit 1
    }
    printf '%s' "$body" >"$tmp"

    if sudo test -f "$file"; then
        if sudo cmp -s "$tmp" "$file"; then
            rm -f "$tmp"
            echo "[INFO] ${svc}: drop-in already up to date ($file)"
            return 0
        fi
    fi

    if sudo mv "$tmp" "$file"; then
        sudo chown root:root "$file" 2>/dev/null || true
        sudo chmod 0644 "$file" 2>/dev/null || true
        echo "[INFO] ${svc}: wrote $file"
        CHANGED=1
        CREATED_FILES="$CREATED_FILES $file"
    else
        rm -f "$tmp"
        echo "[ERROR] Failed to install $file" >&2
        exit 1
    fi
}

# Apply drop-in to all detected services
apply_dropins() {
    CHANGED=0
    CREATED_FILES=""
    for s in $SERVICES_FOUND; do
        deploy_dropin_for "$s"
    done

    if [ "$CHANGED" -eq 1 ]; then
        if sudo systemctl daemon-reload; then
            echo "[INFO] daemon-reload done"
        else
            echo "[ERROR] systemctl daemon-reload failed" >&2
            exit 1
        fi
        echo "[INFO] Created or updated files:${CREATED_FILES}"
    else
        echo "[INFO] No changes; daemon-reload not required"
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands awk grep cmp mktemp mkdir mv rm chmod chown uname systemctl
    check_sudo

    detect_services
    if [ -z "$SERVICES_FOUND" ]; then
        echo "[INFO] No target NTP services found. Nothing to do."
        return 0
    fi

    echo "[INFO] Target services detected:${SERVICES_FOUND}"
    apply_dropins

    echo "[INFO] Current NTP-related services:"
    systemctl list-units --type=service 2>/dev/null | grep ntp || echo "[INFO] No NTP services are active."
    return $?
}

# Execute main function
main "$@"

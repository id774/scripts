#!/bin/sh

########################################################################
# check_reboot.sh: Reboot Requirement Check Tool
#
#  Description:
#  This script checks whether a system reboot is required after updates.
#  It uses two independent signals and reports results with consistent
#  log levels:
#
#  1) /var/run/reboot-required (Debian/Ubuntu mechanism)
#     - If present, a reboot is requested by packaging hooks.
#     - If /var/run/reboot-required.pkgs exists, it lists trigger packages.
#
#  2) needrestart (process-based mechanism, optional)
#     - Detects running processes that still use old binaries/libraries
#       after upgrades and determines whether a reboot is required.
#     - This check is executed only when needrestart is installed and
#       the script runs as root (recommended for accurate inspection).
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Design Notes:
#  - The script is POSIX sh compatible.
#  - It is safe to run on non-Debian systems; reboot-required files may
#    not exist, and needrestart may be unavailable. In that case it will
#    only report what it can determine.
#  - "Reboot required" is a logical OR:
#      reboot_required_file || needrestart_requires_reboot
#
#  Usage:
#      ./check_reboot.sh [options]
#
#  Options:
#      -h, --help:     Show this help and exit.
#      -v, --version:  Show this help and exit (unified behavior).
#
#  Exit Codes:
#      0: Success (check completed; see logs for reboot requirement)
#      1: Command failure or resource missing
#      3: Local prerequisite missing (e.g., non-Linux)
#    126: Command exists but is not executable
#    127: Command not found
#
#  Examples:
#      ./check_reboot.sh
#
#  Version History:
#  v1.0 2026-01-24
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
        exit 3
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

# Check whether a command exists in PATH
have_command() {
    command -v "$1" >/dev/null 2>&1
}

# Determine whether the current user is root
is_root() {
    # Prefer id -u (portable). If id is missing, check_commands covers it.
    [ "$(id -u 2>/dev/null)" = "0" ]
}

# Check Debian/Ubuntu reboot-required marker files
check_reboot_required_files() {
    reboot_flag="/var/run/reboot-required"
    pkgs_file="/var/run/reboot-required.pkgs"

    # When this file exists, Debian/Ubuntu considers a reboot to be required.
    if [ -f "$reboot_flag" ]; then
        echo "[WARN] Reboot required: $reboot_flag exists"

        # The optional .pkgs file contains one package name per line.
        # It may be absent depending on the environment and hooks.
        if [ -f "$pkgs_file" ]; then
            echo "[INFO] Reboot-required packages:"
            # Prefix for consistent log format.
            sed 's/^/[INFO]   - /' "$pkgs_file"
        else
            echo "[INFO] Package list not available: $pkgs_file not found"
        fi

        return 0
    fi

    echo "[INFO] No reboot-required flag: $reboot_flag not found"
    return 1
}

# Check reboot requirement using needrestart (if available)
check_needrestart() {
    # needrestart is optional; skip if not installed.
    if ! have_command needrestart; then
        echo "[INFO] needrestart not installed; skipping needrestart check"
        return 2
    fi

    # needrestart typically needs root for accurate process inspection.
    # Running unprivileged may lead to incomplete results.
    if ! is_root; then
        echo "[WARN] needrestart found but not running as root; skipping needrestart check"
        return 2
    fi

    # Non-interactive and reboot-only check:
    # -r l : check reboot requirement (return 0=no reboot, 1=reboot required)
    # -b   : batch mode (avoid interactive prompts)
    echo "[INFO] Running needrestart (batch, reboot-only)"
    needrestart -r l -b >/dev/null 2>&1
    rc=$?

    case "$rc" in
        0)
            echo "[INFO] needrestart: reboot not required"
            return 1
            ;;
        1)
            echo "[WARN] needrestart: reboot required"
            return 0
            ;;
        *)
            echo "[ERROR] needrestart returned unexpected status: $rc" >&2
            return 3
            ;;
    esac
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands awk sed id uname

    # Track whether any method indicates a reboot requirement.
    reboot_required=0

    # Method 1: reboot-required marker files
    if check_reboot_required_files; then
        reboot_required=1
    fi

    # Method 2: needrestart (optional)
    check_needrestart
    nr_res=$?
    [ "$nr_res" -eq 0 ] && reboot_required=1

    if [ "$reboot_required" -eq 1 ]; then
        echo "[WARN] Reboot is required"
    else
        echo "[INFO] Reboot is not required"
    fi

    return 0
}

# Execute main function
main "$@"

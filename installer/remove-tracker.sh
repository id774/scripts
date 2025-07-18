#!/bin/sh

########################################################################
# remove_tracker.sh: Script to Disable and Remove Tracker from Debian
#
#  Description:
#  This script forcibly stops, disables, and removes all Tracker-related
#  processes and packages from a Debian-based system. Tracker is a file
#  indexing and search service commonly found in GNOME environments.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script directly without any arguments:
#      ./remove_tracker.sh
#
#  To force execution even if tracker is not installed:
#      ./remove_tracker.sh -f
#
#  Requirements:
#  - Must be run on a Debian-based system.
#  - Requires sudo privileges to remove system packages.
#  - systemd should be available for service management.
#
#  Notes:
#  - This script aggressively stops and removes all Tracker components,
#    including those required for GNOME search functionality.
#  - If Tracker is needed for file search, consider disabling it instead
#    of full removal.
#  - Since package names vary by Debian version, the script attempts to
#    remove multiple variations.
#  - Services are masked to prevent automatic re-enablement in the future.
#  - The script does not prompt for confirmation when removing packages.
#  - If any package removal fails, review the output logs for troubleshooting.
#  - The `-f` option forces execution even if Tracker is not detected.
#    This can be useful if you want to remove residual packages or mask
#    systemd services regardless of installation status.
#
#  Version History:
#  v1.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.3 2025-03-16
#       Added Linux OS check, systemd check, and command validation.
#       Improved error handling and robustness.
#  v1.2 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.1 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.0 2025-02-04
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
    if [ "$(uname -s)" != "Linux" ]; then
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

# Initial checks and setup
perform_initial_checks() {
    check_system
    check_commands systemd dpkg pgrep pkill apt systemctl
    check_sudo
}

# Main operations to stop, disable, and remove tracker
perform_tracker_operations() {
    stop_tracker_processes
    disable_tracker_services
    reset_tracker_database
    purge_tracker_packages
    prevent_tracker_reinstallation
    cleanup_system
}

# Stop all tracker-related processes
stop_tracker_processes() {
    echo "[INFO] Stopping all tracker-related processes..."
    for PROCESS in tracker tracker3 tracker-miner-fs tracker-extract \
                   tracker-miner-apps tracker-miner-rss tracker-store \
                   tracker-daemon tracker-indexer; do
        if pgrep -x "$PROCESS" >/dev/null 2>&1; then
            echo "[INFO] Stopping process: $PROCESS"
            pkill -9 "$PROCESS"
        fi
    done
}

# Disable systemd user services for tracker
disable_tracker_services() {
    echo "[INFO] Disabling tracker services..."
    for SERVICE in tracker.service tracker3.service tracker-miner-fs.service \
                   tracker-miner-fs-3.service tracker-miner-rss.service; do
        systemctl --user stop "$SERVICE" 2>/dev/null
        systemctl --user disable "$SERVICE" 2>/dev/null
        systemctl --user mask "$SERVICE" 2>/dev/null
    done
}

# Reset the tracker database
reset_tracker_database() {
    echo "[INFO] Resetting tracker database..."
    if command -v tracker3 >/dev/null 2>&1; then
        tracker3 reset --hard
    elif command -v tracker >/dev/null 2>&1; then
        tracker reset --hard
    fi
}

# Purge all tracker-related packages
purge_tracker_packages() {
    echo "[INFO] Purging tracker-related packages..."
    for PACKAGE in tracker tracker3 tracker-miner-fs tracker-extract \
                   tracker-gui tracker-miners tracker-store tracker-indexer \
                   tracker-doc libtracker-control-2.0-0 libtracker-control-3-0 \
                   libtracker-miner-2.0-0 libtracker-miner-3-0 \
                   libtracker-sparql-2.0-0 libtracker-sparql-3-0; do
        if dpkg -l | grep -q "^ii  $PACKAGE "; then
            echo "[INFO] Purging package: $PACKAGE"
            sudo apt purge -y "$PACKAGE"
        fi
    done
}

# Prevent reinstallation of tracker packages
prevent_tracker_reinstallation() {
    echo "[INFO] Running cleanup operations..."
    for PACKAGE in tracker tracker3 tracker-miner-fs tracker-extract \
                   tracker-gui tracker-miners tracker-store tracker-indexer \
                   tracker-doc libtracker-control-2.0-0 libtracker-control-3-0 \
                   libtracker-miner-2.0-0 libtracker-miner-3-0 \
                   libtracker-sparql-2.0-0 libtracker-sparql-3-0; do
        sudo apt-mark hold "$PACKAGE"
    done
}

# Cleanup system by removing orphaned packages and cache
cleanup_system() {
    sudo apt autoremove -y
    sudo apt autoclean -y
    sudo apt clean -y
}

# Parse command-line options
parse_options() {
    FORCE_REMOVE=0
    while getopts "f" opt; do
        case "$opt" in
            f) FORCE_REMOVE=1 ;;
            *) exit 1 ;;
        esac
    done
    shift $((OPTIND -1))
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    parse_options "$@"
    perform_initial_checks
    if [ "$FORCE_REMOVE" -eq 0 ] && ! command -v tracker3 >/dev/null 2>&1 && ! command -v tracker >/dev/null 2>&1; then
        echo "[ERROR] tracker is not installed." >&2
        exit 1
    fi
    perform_tracker_operations
    echo "[INFO] Tracker has been completely removed and cleaned up."
    return 0
}

# Execute main function
main "$@"
exit $?

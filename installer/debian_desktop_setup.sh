#!/bin/sh

########################################################################
# debian_desktop_setup.sh: Debian batch setup script for Desktop
#
#  Description:
#  This script configures essential settings for Debian-based desktop environments.
#  It disables guest sessions in LightDM and restarts the display manager to apply changes.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-13
#       Automated guest session disabling in LightDM.
#       Improved script automation and removed manual editing step.
#       Added system checks and improved error handling.
#       Redirected error messages to stderr for better logging and debugging.
#  [Further version history truncated for brevity]
#  v0.1 2011-09-28
#       First version.
#
#  Usage:
#  Run the script directly:
#      ./debian_desktop_setup.sh
#
#  Ensure that LightDM is installed before executing.
#
#  Notes:
#  - The script is designed for Debian-based systems using LightDM.
#  - Manual verification of /etc/lightdm/lightdm.conf may be required.
#  - Review and modify configurations as needed before execution.
#
#  Error Conditions:
#  - If LightDM is not installed, the script exits with an error.
#  - If required commands are missing, execution is halted.
#  - Errors from underlying commands should be resolved based on their output.
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

# Function to check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Function to check required commands
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

# Check if a desktop environment is installed
check_desktop_installed() {
    if tasksel --list-tasks | grep -q '^i.*desktop'; then
        echo "[INFO] Desktop environment detected."
    else
        echo "[ERROR] No desktop environment found. Please install a desktop environment before running this script." >&2
        exit 1
    fi
}

# Function to check if LightDM is installed
check_lightdm() {
    if ! dpkg-query -W -f='${Status}' lightdm 2>/dev/null | grep -q "ok installed"; then
        echo "[ERROR] LightDM is not installed. This script requires LightDM." >&2
        exit 1
    fi
}

# Disable guest sessions in LightDM
disable_guest_session() {
    LIGHTDM_CONF="/etc/lightdm/lightdm.conf"
    sudo mkdir -p /etc/lightdm
    if ! grep -q "^allow-guest=false" "$LIGHTDM_CONF" 2>/dev/null; then
        echo "[INFO] Disabling guest sessions in LightDM..."
        echo "allow-guest=false" | sudo tee -a "$LIGHTDM_CONF" >/dev/null
    else
        echo "[INFO] Guest sessions are already disabled in LightDM."
    fi
}

# Restart LightDM to apply changes
restart_lightdm() {
    echo "[INFO] Restarting LightDM..."
    sudo systemctl restart lightdm || echo "[WARN] Failed to restart LightDM." >&2
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_commands sudo dpkg-query grep tee systemctl tasksel
    check_desktop_installed
    check_lightdm
    check_sudo
    disable_guest_session
    restart_lightdm

    echo "[INFO] All Debian desktop setup completed."
    return 0
}

# Execute main function
main "$@"
exit $?

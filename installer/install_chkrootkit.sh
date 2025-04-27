#!/bin/sh

########################################################################
# install_chkrootkit.sh: chkrootkit Setup Script
#
#  Description:
#  This script sets up automated rootkit detection using chkrootkit by deploying
#  the chkrootkit script and configuring necessary cron jobs and log rotation.
#  It ensures that the required directories and log files exist, sets appropriate
#  permissions, and deploys cron jobs for rootkit detection.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.5 2025-04-27
#       Simplified error checking to critical filesystem operations.
#  v1.4 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v1.3 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.2 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.1 2025-03-14
#       Added command existence check, system check, and environment variable validation.
#  v1.0 2012-05-15
#       Initial release.
#
#  Usage:
#      ./install_chkrootkit.sh
#
#  Notes:
#  - Ensure the SCRIPTS environment variable is set to the directory containing
#    the chkrootkit script and its related files before running this script.
#
########################################################################

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
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

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the chkrootkit script." >&2
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

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    # Perform initial checks
    check_system
    check_commands sudo cp chmod chown mkdir touch
    check_scripts
    check_sudo

    # Deploy chkrootkit cron job
    echo "[INFO] Deploying chkrootkit cron job."
    if ! sudo cp "$SCRIPTS/cron/bin/chkrootkit" /etc/cron.weekly/chkrootkit; then
        echo "[ERROR] Failed to copy chkrootkit cron script." >&2
        exit 1
    fi
    sudo chmod 740 /etc/cron.weekly/chkrootkit
    sudo chown root:adm /etc/cron.weekly/chkrootkit

    # Create log directory if it does not exist
    echo "[INFO] Ensuring /var/log/chkrootkit directory exists."
    if [ ! -d /var/log/chkrootkit ]; then
        if ! sudo mkdir -p /var/log/chkrootkit; then
            echo "[ERROR] Failed to create /var/log/chkrootkit directory." >&2
            exit 1
        fi
    fi
    sudo chmod 750 /var/log/chkrootkit
    sudo chown root:adm /var/log/chkrootkit

    # Set up chkrootkit log file
    echo "[INFO] Initializing chkrootkit log file."
    if [ ! -f /var/log/chkrootkit/chkrootkit.log ]; then
        sudo touch /var/log/chkrootkit/chkrootkit.log
    fi
    sudo chmod 640 /var/log/chkrootkit/chkrootkit.log
    sudo chown root:adm /var/log/chkrootkit/chkrootkit.log

    # Deploy log rotation configuration
    echo "[INFO] Deploying logrotate configuration for chkrootkit."
    if ! sudo cp "$SCRIPTS/cron/etc/logrotate.d/chkrootkit" /etc/logrotate.d/chkrootkit; then
        echo "[ERROR] Failed to deploy logrotate configuration." >&2
        exit 1
    fi
    sudo chmod 644 /etc/logrotate.d/chkrootkit
    sudo chown root:root /etc/logrotate.d/chkrootkit

    echo "[INFO] chkrootkit setup completed successfully."
}

# Execute main function
main "$@"

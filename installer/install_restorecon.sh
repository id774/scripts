#!/bin/sh

########################################################################
# install_restorecon.sh: Setup Restorecon Cron Job
#
#  Description:
#  This script sets up a cron job to run restorecon periodically, ensuring
#  that SELinux context is correctly applied to files. It creates necessary
#  directories, deploys the restorecon.sh script, sets up a weekly cron job,
#  and configures log rotation for the job's log files.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.7 2025-04-26
#       Add critical failure checks during restorecon setup.
#  v0.6 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v0.5 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v0.4 2025-03-14
#       Added Linux system validation, command validation, and improved error handling.
#  v0.3 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v0.2 2024-03-17
#       Refactored for improved readability and maintainability.
#  v0.1 2014-06-02
#       Initial setup of restorecon cron job.
#
#  Usage:
#      ./install_restorecon.sh
#
#  Notes:
#  - Ensure the SCRIPTS environment variable is set to the directory containing
#    the restorecon script and configuration files before running this script.
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

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the restorecon script." >&2
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
    check_commands sudo cp chmod chown touch mkdir
    check_scripts
    check_sudo

    echo "[INFO] Setting up directories..."
    if [ ! -d /var/log/sysadmin ]; then
        sudo mkdir -p /var/log/sysadmin || {
            echo "[ERROR] Failed to create /var/log/sysadmin." >&2
            exit 1
        }
        sudo chmod 750 /var/log/sysadmin
        sudo chown root:adm /var/log/sysadmin
    fi

    echo "[INFO] Deploying restorecon script..."
    if ! sudo cp "$SCRIPTS/cron/bin/restorecon.sh" /root/bin/; then
        echo "[ERROR] Failed to deploy restorecon.sh." >&2
        exit 1
    fi
    sudo chmod 700 /root/bin/restorecon.sh
    sudo chown root:root /root/bin/restorecon.sh

    echo "[INFO] Setting up restorecon cron job..."
    if ! sudo cp "$SCRIPTS/cron/bin/restorecon" /etc/cron.weekly/; then
        echo "[ERROR] Failed to deploy restorecon cron job." >&2
        exit 1
    fi
    sudo chmod 750 /etc/cron.weekly/restorecon
    sudo chown root:adm /etc/cron.weekly/restorecon

    echo "[INFO] Preparing restorecon log file..."
    if [ ! -f /var/log/sysadmin/restorecon.log ]; then
        if ! sudo touch /var/log/sysadmin/restorecon.log; then
            echo "[ERROR] Failed to create restorecon.log." >&2
            exit 1
        fi
        sudo chmod 640 /var/log/sysadmin/restorecon.log
        sudo chown root:adm /var/log/sysadmin/restorecon.log
    fi

    echo "[INFO] Deploying logrotate configuration..."
    if [ ! -f /etc/logrotate.d/restorecon ]; then
        if ! sudo cp "$SCRIPTS/cron/etc/logrotate.d/restorecon" /etc/logrotate.d/; then
            echo "[ERROR] Failed to deploy logrotate configuration." >&2
            exit 1
        fi
        sudo chmod 644 /etc/logrotate.d/restorecon
        sudo chown root:root /etc/logrotate.d/restorecon
    fi

    echo "[INFO] Restorecon cron job setup completed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.4 2025-03-14
#       Added Linux system validation, command validation, and improved error handling.
#  v0.3 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v0.2 2024-03-17
#       Refactored for improved readability and maintainability.
#  v0.1 2014-06-02
#       Initial setup of restorecon cron job.
#
#  Notes:
#  - Ensure the SCRIPTS environment variable is set to the directory containing
#    the restorecon script and configuration files before running this script.
#
########################################################################

# Function to check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "Error: This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the restorecon script." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Main execution function
main() {
    # Perform initial checks
    check_system
    check_commands sudo cp chmod chown touch mkdir
    check_scripts
    check_sudo

    # Ensure required directory exists
    if [ ! -d /var/log/sysadmin ]; then
        sudo mkdir -p /var/log/sysadmin
        sudo chmod 750 /var/log/sysadmin
        sudo chown root:adm /var/log/sysadmin
    fi

    # Deploy restorecon script
    sudo cp "$SCRIPTS/cron/bin/restorecon.sh" /root/bin/
    sudo chmod 700 /root/bin/restorecon.sh
    sudo chown root:root /root/bin/restorecon.sh

    # Set up restorecon cron job
    sudo cp "$SCRIPTS/cron/bin/restorecon" /etc/cron.weekly/
    sudo chmod 750 /etc/cron.weekly/restorecon
    sudo chown root:adm /etc/cron.weekly/restorecon

    # Ensure restorecon log file exists and set permissions
    if [ ! -f /var/log/sysadmin/restorecon.log ]; then
        sudo touch /var/log/sysadmin/restorecon.log
        sudo chmod 640 /var/log/sysadmin/restorecon.log
        sudo chown root:adm /var/log/sysadmin/restorecon.log
    fi

    # Deploy log rotation configuration for restorecon logs
    if [ ! -f /etc/logrotate.d/restorecon ]; then
        sudo cp "$SCRIPTS/cron/etc/logrotate.d/restorecon" /etc/logrotate.d/
        sudo chmod 644 /etc/logrotate.d/restorecon
        sudo chown root:root /etc/logrotate.d/restorecon
    fi

    echo "Restorecon cron job setup completed successfully."
}

main "$@"

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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-14
#       Added command existence check, system check, and environment variable validation.
#  v1.0 2012-05-15
#       Initial release.
#
#  Notes:
#  - Ensure the SCRIPTS environment variable is set to the directory containing
#    the chkrootkit script and its related files before running this script.
#
########################################################################

# Check if the system is Linux
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

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the chkrootkit script." >&2
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

# Perform initial checks
check_system
check_commands sudo cp chmod chown mkdir touch
check_scripts
check_sudo

# Deploy chkrootkit cron job
sudo cp "$SCRIPTS/cron/bin/chkrootkit" /etc/cron.weekly/chkrootkit
sudo chmod 750 /etc/cron.weekly/chkrootkit
sudo chown root:adm /etc/cron.weekly/chkrootkit

# Create log directory if it does not exist
if [ ! -d /var/log/chkrootkit ]; then
    sudo mkdir -p /var/log/chkrootkit
    sudo chmod 750 /var/log/chkrootkit
    sudo chown root:adm /var/log/chkrootkit
fi

# Set up chkrootkit log file and permissions
if [ ! -f /var/log/chkrootkit/chkrootkit.log ]; then
    sudo touch /var/log/chkrootkit/chkrootkit.log
    sudo chmod 640 /var/log/chkrootkit/chkrootkit.log
    sudo chown root:adm /var/log/chkrootkit/chkrootkit.log
fi

# Deploy log rotation configuration
sudo cp "$SCRIPTS/cron/etc/logrotate.d/chkrootkit" /etc/logrotate.d/chkrootkit
sudo chmod 644 /etc/logrotate.d/chkrootkit
sudo chown root:root /etc/logrotate.d/chkrootkit

echo "chkrootkit setup completed successfully."

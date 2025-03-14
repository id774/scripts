#!/bin/sh

########################################################################
# install_rsync_backup.sh: Rsync Backup Setup Script
#
#  Description:
#  This script sets up automated backups using rsync by deploying the
#  rsync_backup.sh script and its configuration, setting up cron jobs for
#  regular backups, and managing log rotation for backup logs. It ensures that
#  the necessary directories and log files exist, sets appropriate permissions,
#  and deploys cron jobs and log rotation configurations.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.1 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v2.0 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.9 2024-03-17
#       Refactored script for improved readability and maintainability.
#  v1.8 2023-12-17
#       Refactor rsync_backup script for modular operation definitions.
#  [Further version history truncated for brevity]
#
#  Notes:
#  - Ensure the SCRIPTS environment variable is set to the directory containing
#    the rsync backup scripts and configuration files before running this script.
#
########################################################################

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Ensure SCRIPTS environment variable is set
if [ -z "$SCRIPTS" ]; then
    echo "SCRIPTS environment variable is not set. Please set it to the directory containing the rsync backup related files." >&2
    exit 1
fi

check_sudo

# Make Directory if it doesn't exist and set permissions
if [ ! -d /var/log/sysadmin ]; then
    sudo mkdir -p /var/log/sysadmin
    sudo chmod 750 /var/log/sysadmin
    sudo chown root:adm /var/log/sysadmin
fi

# Deploy rsync backup script and configuration
sudo cp "$SCRIPTS/cron/bin/rsync_backup.sh" /root/bin/
sudo chmod 700 /root/bin/rsync_backup.sh
sudo chown root:root /root/bin/rsync_backup.sh

sudo cp "$SCRIPTS/cron/etc/rsync_backup.conf" /root/etc/
sudo chmod 600 /root/etc/rsync_backup.conf
sudo chown root:root /root/etc/rsync_backup.conf

# Deploy rsync backup cron job
sudo cp "$SCRIPTS/cron/bin/rsync_backup" /etc/cron.hourly/
sudo chmod 750 /etc/cron.hourly/rsync_backup
sudo chown root:adm /etc/cron.hourly/rsync_backup

# Set up rsync backup log file and permissions
if [ ! -f /var/log/sysadmin/rsync_backup ]; then
    sudo touch /var/log/sysadmin/rsync_backup
    sudo chmod 640 /var/log/sysadmin/rsync_backup
    sudo chown root:adm /var/log/sysadmin/rsync_backup
fi

# Deploy log rotation configuration for rsync backup logs
if [ ! -f /etc/logrotate.d/rsync_backup ]; then
    sudo cp "$SCRIPTS/cron/etc/logrotate.d/rsync_backup" /etc/logrotate.d/
    sudo chmod 644 /etc/logrotate.d/rsync_backup
    sudo chown root:root /etc/logrotate.d/rsync_backup
fi

echo "Rsync backup setup completed successfully."

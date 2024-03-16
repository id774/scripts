#!/bin/sh

########################################################################
# install_clamscan.sh: ClamAV AutoScan Setup Script
#
#  Description:
#  This script automates the setup for ClamAV scans by deploying the
#  clamscan.sh script, configuring clamscan exclusions, setting up cron jobs for
#  weekend scanning, and managing log rotation for ClamAV logs. It ensures the
#  necessary directories and log files exist, sets appropriate permissions, and
#  deploys the cron jobs and log rotation configurations.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.6 2024-03-17
#       Refactored script for improved readability and maintainability.
#  [Further version history truncated for brevity]
#
#  Notes:
#  - Ensure the SCRIPTS environment variable is set to the directory containing
#    the clamscan related scripts and configuration files before running this script.
#
########################################################################

# Ensure SCRIPTS environment variable is set
if [ -z "$SCRIPTS" ]; then
    echo "SCRIPTS environment variable is not set. Please set it to the directory containing the clamscan related files."
    exit 1
fi

# Make Directory if it doesn't exist and set permissions
if [ ! -d /var/log/sysadmin ]; then
    sudo mkdir -p /var/log/sysadmin
    sudo chmod 750 /var/log/sysadmin
    sudo chown root:adm /var/log/sysadmin
fi

# Deploy clamscan script and exclusion file
sudo cp "$SCRIPTS/cron/bin/clamscan.sh" /root/bin/
sudo chmod 700 /root/bin/clamscan.sh
sudo chown root:root /root/bin/clamscan.sh

sudo cp "$SCRIPTS/cron/etc/clamscan_exclude" /root/bin/
sudo chmod 600 /root/bin/clamscan_exclude
sudo chown root:root /root/bin/clamscan_exclude

# Deploy clamscan cron job
sudo cp "$SCRIPTS/cron/bin/clamscan" /etc/cron.weekly/
sudo chmod 740 /etc/cron.weekly/clamscan
sudo chown root:adm /etc/cron.weekly/clamscan

# Set up ClamAV log files and permissions
for log_file in /var/log/clamav/clamscan.log /var/log/clamav/clamav.log; do
    if [ ! -f "$log_file" ]; then
        sudo touch "$log_file"
        sudo chmod 640 "$log_file"
        sudo chown clamav:adm "$log_file"
    fi
done

# Deploy log rotation configuration for ClamAV logs
if [ ! -f /etc/logrotate.d/clamscan ]; then
    sudo cp "$SCRIPTS/cron/etc/logrotate.d/clamscan" /etc/logrotate.d/
    sudo chmod 640 /etc/logrotate.d/clamscan
    sudo chown root:adm /etc/logrotate.d/clamscan
fi

echo "ClamAV AutoScan setup completed successfully."

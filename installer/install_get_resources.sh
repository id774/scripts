#!/bin/sh

########################################################################
# install_get_resources.sh: Server Resource Report Setup Script
#
#  Description:
#  This script sets up automated server resource reporting by deploying the
#  get_resources.sh script and configuring necessary cron jobs and log rotation.
#  It ensures that the required directories and log files exist, sets appropriate
#  permissions, and deploys cron jobs for resource reporting.
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
#  v1.0 2008-08-15
#       Stable initial release.
#
#  Notes:
#  - Ensure the SCRIPTS environment variable is set to the directory containing
#    the get_resources script and its related files before running this script.
#
########################################################################

# Ensure SCRIPTS environment variable is set
if [ -z "$SCRIPTS" ]; then
    echo "SCRIPTS environment variable is not set. Please set it to the directory containing the get_resources script."
    exit 1
fi

# Make Directory if it doesn't exist and set permissions
if [ ! -d /var/log/sysadmin ]; then
    sudo mkdir -p /var/log/sysadmin
    sudo chmod 750 /var/log/sysadmin
    sudo chown root:adm /var/log/sysadmin
fi

# Set up log file and permissions
if [ ! -f /var/log/sysadmin/resources.log ]; then
    sudo touch /var/log/sysadmin/resources.log
    sudo chmod 640 /var/log/sysadmin/resources.log
    sudo chown root:adm /var/log/sysadmin/resources.log
fi

# Deploy log rotation configuration
if [ ! -f /etc/logrotate.d/resources ]; then
    sudo cp "$SCRIPTS/cron/etc/logrotate.d/resources" /etc/logrotate.d/resources
    sudo chmod 640 /etc/logrotate.d/resources
    sudo chown root:adm /etc/logrotate.d/resources
fi

# Deploy the get_resources script and cron job
sudo cp "$SCRIPTS/get_resources.sh" /root/bin/
sudo chmod 700 /root/bin/get_resources.sh
sudo chown root:root /root/bin/get_resources.sh

sudo cp "$SCRIPTS/cron/bin/get_resources" /etc/cron.hourly/
sudo chmod 740 /etc/cron.hourly/get_resources
sudo chown root:adm /etc/cron.hourly/get_resources

echo "Server resource report setup completed successfully."

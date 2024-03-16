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
#  Source Code: (If available, provide URL)
#  License: (Specify if applicable)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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

# Ensure SCRIPTS environment variable is set
if [ -z "$SCRIPTS" ]; then
    echo "SCRIPTS environment variable is not set. Please set it to the directory containing the restorecon script."
    exit 1
fi

# Make Directory if it doesn't exist and set permissions
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

# Set up restorecon log file and permissions
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

#!/bin/sh

########################################################################
# install_run_tests.sh: Deploy run_tests.sh and Configure Cron Job
#
#  Description:
#  This script automates the deployment of the run_tests.sh script and its
#  configuration file for automated testing of Python and Ruby projects.
#  It ensures the necessary directories exist, copies the script and its
#  configuration file to secure locations, sets appropriate permissions,
#  and schedules the tests to run automatically via a cron job.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2024-03-17
#       Added log file creation in /var/log/sysadmin and deployed log rotation configuration.
#  v1.0 2024-03-13
#       Initial deployment script for automated testing setup.
#
#  Notes:
#  - The SCRIPTS environment variable must be set to the directory containing
#    the run_tests script and its configuration file before running this script.
#  - After deployment, review and potentially edit /root/etc/run_tests.conf
#    and /etc/cron.d/run_tests to finalize the configuration.
#
#  Usage:
#  Execute this script with sufficient permissions to perform directory
#  creation, file copying, and cron job scheduling tasks:
#      ./install_run_tests.sh
#
########################################################################

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access."
        exit 1
    fi
}

# Ensure SCRIPTS environment variable is set
if [ -z "$SCRIPTS" ]; then
    echo "SCRIPTS environment variable is not set. Please set it to the directory containing the run_tests script."
    exit 1
fi

check_sudo

# Make Directory if it doesn't exist and set permissions
if [ ! -d /var/log/sysadmin ]; then
    sudo mkdir -p /var/log/sysadmin
    sudo chmod 750 /var/log/sysadmin
    sudo chown root:adm /var/log/sysadmin
fi

# Set up log file and permissions
if [ ! -f /var/log/sysadmin/run_tests.log ]; then
    sudo touch /var/log/sysadmin/run_tests.log
    sudo chmod 640 /var/log/sysadmin/run_tests.log
    sudo chown root:adm /var/log/sysadmin/run_tests.log
fi

# Deploy log rotation configuration
if [ ! -f /etc/logrotate.d/run_tests ]; then
    sudo cp "$SCRIPTS/cron/etc/logrotate.d/run_tests" /etc/logrotate.d/run_tests
    sudo chmod 640 /etc/logrotate.d/run_tests
    sudo chown root:adm /etc/logrotate.d/run_tests
fi

# Deploy the run_tests script and its configuration file
sudo cp "$SCRIPTS/cron/bin/run_tests" /root/bin/
sudo cp "$SCRIPTS/cron/etc/run_tests.conf" /root/etc/

# Set permissions for the script and configuration file
sudo chmod 740 /root/bin/run_tests
sudo chown root:root /root/bin/run_tests
sudo chmod 640 /root/etc/run_tests.conf
sudo chown root:root /root/etc/run_tests.conf

# Prepare the cron job file
CRON_JOB="30 22 * * * root test -x /root/bin/run_tests && /root/bin/run_tests"
echo "$CRON_JOB" | sudo tee /etc/cron.d/run_tests > /dev/null

# Note: Manual editing of '/root/etc/run_tests.conf' and '/etc/cron.d/run_tests' may be required
# to finalize configurations. Please review and edit these files as necessary.


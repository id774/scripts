#!/bin/sh

########################################################################
# install_apache_log_analysis.sh: Setup Apache Log Analysis
#
#  Description:
#  This script sets up Apache log analysis by:
#  - Deploying analysis scripts and configurations.
#  - Configuring scheduled cron jobs for log analysis.
#  - Ensuring proper file and directory permissions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-03-15
#       Refactored script with improved checks and error handling.
#       Added functions for modular execution.
#       Ensured idempotent directory and file setup.
#  v1.2 2023-12-25
#       Add apache_calculater.
#  v1.1 2023-12-16
#       Also install ignore list.
#  v1.0 2022-10-11
#       Stable.
#
#  Usage:
#  Run this script to set up Apache log analysis:
#      ./install_apache_log_analysis.sh
#
#  Requirements:
#  - Must be executed with sudo privileges.
#  - The `$SCRIPTS` environment variable must be set.
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
        echo "Please set the SCRIPTS variable to the directory containing Apache log analysis scripts." >&2
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

# Function to create required directories with proper permissions
setup_directories() {
    echo "Setting up directories..."
    if [ ! -d /var/log/sysadmin ]; then
        sudo mkdir -p /var/log/sysadmin
        sudo chmod 750 /var/log/sysadmin
        sudo chown root:adm /var/log/sysadmin
    fi
}

# Function to deploy Apache log analysis scripts
deploy_scripts() {
    echo "Deploying log analysis scripts..."
    for script in apache_log_analysis.sh apache_calculater.py; do
        sudo cp "$SCRIPTS/$script" "/root/bin/$script"
        sudo chmod 700 "/root/bin/$script"
        sudo chown root:root "/root/bin/$script"
    done
}

# Function to deploy configuration files
deploy_configurations() {
    echo "Deploying configuration files..."
    sudo cp "$SCRIPTS/etc/apache_ignore.list" /root/etc/apache_ignore.list
    sudo chmod 600 /root/etc/apache_ignore.list
    sudo chown root:root /root/etc/apache_ignore.list
}

# Function to deploy cron jobs
setup_cron_jobs() {
    echo "Setting up cron jobs..."
    sudo cp "$SCRIPTS/cron/bin/apache_log_analysis" /etc/cron.daily/apache_log_analysis
    sudo chmod 750 /etc/cron.daily/apache_log_analysis
    sudo chown root:adm /etc/cron.daily/apache_log_analysis
}

# Function to set up log files
setup_log_files() {
    echo "Setting up log files..."
    if [ ! -f /var/log/sysadmin/apache_summary.log ]; then
        sudo touch /var/log/sysadmin/apache_summary.log
        sudo chmod 640 /var/log/sysadmin/apache_summary.log
        sudo chown root:adm /var/log/sysadmin/apache_summary.log
    fi
}

# Function to deploy log rotation configuration
setup_log_rotation() {
    echo "Deploying log rotation configuration..."
    sudo cp "$SCRIPTS/cron/etc/logrotate.d/apache_summary" /etc/logrotate.d/apache_summary
    sudo chmod 644 /etc/logrotate.d/apache_summary
    sudo chown root:root /etc/logrotate.d/apache_summary
}

# Main execution function
main() {
    check_system
    check_commands sudo cp chmod chown mkdir touch
    check_scripts
    check_sudo

    setup_directories
    deploy_scripts
    deploy_configurations
    setup_cron_jobs
    setup_log_files
    setup_log_rotation

    echo "Apache log analysis setup completed successfully."
}

main "$@"

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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.5 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.4 2025-03-21
#       Refactored script by functionizing all processes without changing behavior.
#  v1.3 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.2 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.1 2024-03-17
#       Added log file creation in /var/log/sysadmin and deployed log rotation configuration.
#  v1.0 2024-03-13
#       Initial deployment script for automated testing setup.
#
#  Usage:
#  Execute this script with sufficient permissions to perform directory
#  creation, file copying, and cron job scheduling tasks:
#      ./install_run_tests.sh
#
#  Notes:
#  - The SCRIPTS environment variable must be set to the directory containing
#    the run_tests script and its configuration file before running this script.
#  - After deployment, review and potentially edit /root/etc/run_tests.conf
#    and /etc/cron.d/run_tests to finalize the configuration.
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
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set." >&2
        echo "Please set it to the directory containing the run_tests script." >&2
        exit 1
    fi
}

# Create directory for logs
setup_log_directory() {
    if [ ! -d /var/log/sysadmin ]; then
        sudo mkdir -p /var/log/sysadmin || exit 1
        sudo chmod 750 /var/log/sysadmin || exit 1
        sudo chown root:adm /var/log/sysadmin || exit 1
    fi
}

# Set up log file and permissions
setup_log_file() {
    if [ ! -f /var/log/sysadmin/run_tests.log ]; then
        sudo touch /var/log/sysadmin/run_tests.log || exit 1
        sudo chmod 640 /var/log/sysadmin/run_tests.log || exit 1
        sudo chown root:adm /var/log/sysadmin/run_tests.log || exit 1
    fi
}

# Deploy log rotation configuration
deploy_log_rotation() {
    if [ ! -f /etc/logrotate.d/run_tests ]; then
        sudo cp "$SCRIPTS/cron/etc/logrotate.d/run_tests" /etc/logrotate.d/run_tests || exit 1
        sudo chmod 640 /etc/logrotate.d/run_tests || exit 1
        sudo chown root:adm /etc/logrotate.d/run_tests || exit 1
    fi
}

# Deploy run_tests script and configuration file
deploy_scripts() {
    sudo cp "$SCRIPTS/cron/bin/run_tests" /root/bin/ || exit 1
    sudo cp "$SCRIPTS/cron/etc/run_tests.conf" /root/etc/ || exit 1
    sudo chmod 740 /root/bin/run_tests || exit 1
    sudo chown root:root /root/bin/run_tests || exit 1
    sudo chmod 640 /root/etc/run_tests.conf || exit 1
    sudo chown root:root /root/etc/run_tests.conf || exit 1
}

# Set up cron job for running tests
setup_cron_job() {
    CRON_JOB="30 22 * * * root test -x /root/bin/run_tests && /root/bin/run_tests"
    echo "$CRON_JOB" | sudo tee /etc/cron.d/run_tests > /dev/null || exit 1
}

# Main function to execute all setup tasks
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_scripts
    check_commands sudo cp chmod chown touch mkdir tee
    check_sudo
    setup_log_directory
    setup_log_file
    deploy_log_rotation
    deploy_scripts
    setup_cron_job
    echo "Installation of run_tests setup completed successfully."
    echo "# Notes: Manual editing of '/root/etc/run_tests.conf' and '/etc/cron.d/run_tests' may be required"
    echo "# to finalize configurations. Please review and edit these files as necessary."
}

# Execute main function
main "$@"

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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.0 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.9 2025-03-14
#       Added command existence check, system check, and environment variable validation.
#  v1.8 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.7 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.6 2024-03-17
#       Refactored script for improved readability and maintainability.
#  [Further version history truncated for brevity]
#  v1.0 2008-08-15
#       Stable initial release.
#
#  Usage:
#      ./install_get_resources.sh
#
#  Notes:
#  - Ensure the SCRIPTS environment variable is set to the directory containing
#    the get_resources script and its related files before running this script.
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

# Check if the system is Linux
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

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the get_resources script." >&2
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
    check_commands sudo cp chmod chown mkdir touch
    check_scripts
    check_sudo

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
}

# Execute main function
main "$@"

#!/bin/sh

########################################################################
# install_fix-permissions.sh: Installer for fix-permissions Script
#
#  Description:
#  This script automates the setup of the `fix-permissions` script by:
#  - Ensuring the required logging directory and file exist with
#    appropriate permissions.
#  - Deploying a log rotation configuration to manage log size.
#  - Installing the `fix-permissions` script as a daily cron job.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.5 2025-04-11
#       Restricted permissions of fix-permissions cron job to root and adm only.
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.3 2025-03-14
#       Added command existence check, system check, and environment variable validation.
#  v1.2 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.1 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.0 2024-12-09
#       Initial release with support for logging setup, log rotation, and cron job installation.
#
#  Usage:
#  Run this script directly without any arguments:
#      ./install_fix-permissions.sh
#
#  Requirements:
#  - The `SCRIPTS` environment variable must be set to the directory
#    containing the `fix-permissions` script and its configurations.
#  - Must be executed with sufficient permissions to modify system
#    directories (typically as root or with sudo).
#  - Requires `logrotate` to be installed for log rotation setup.
#
#  Notes:
#  - The script ensures that `/var/log/sysadmin` is created if it does
#    not exist and configures it securely.
#  - If a log rotation configuration for `fix-permissions` already exists,
#    it will not be overwritten.
#  - The `fix-permissions` script is deployed to `/etc/cron.daily` with
#    appropriate permissions.
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
        echo "Please set the SCRIPTS variable to the directory containing the fix-permissions script." >&2
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
    check_commands sudo cp chmod chown mkdir touch logrotate
    check_scripts
    check_sudo

    # Make Directory if it doesn't exist and set permissions
    if [ ! -d /var/log/sysadmin ]; then
        sudo mkdir -p /var/log/sysadmin
        sudo chmod 750 /var/log/sysadmin
        sudo chown root:adm /var/log/sysadmin
    fi

    # Set up log file and permissions
    if [ ! -f /var/log/sysadmin/fix-permissions.log ]; then
        sudo touch /var/log/sysadmin/fix-permissions.log
        sudo chmod 640 /var/log/sysadmin/fix-permissions.log
        sudo chown root:adm /var/log/sysadmin/fix-permissions.log
    fi

    # Deploy log rotation configuration
    if [ ! -f /etc/logrotate.d/fix-permissions ]; then
        sudo cp "$SCRIPTS/cron/etc/logrotate.d/fix-permissions" /etc/logrotate.d/fix-permissions
        sudo chmod 640 /etc/logrotate.d/fix-permissions
        sudo chown root:adm /etc/logrotate.d/fix-permissions
    fi

    # Deploy the fix-permissions script and cron job
    sudo cp "$SCRIPTS/cron/bin/fix-permissions.sh" /etc/cron.daily/fix-permissions
    sudo chmod 740 /etc/cron.daily/fix-permissions
    sudo chown root:adm /etc/cron.daily/fix-permissions

    echo "[INFO] Fix-permissions script setup completed successfully."
}

# Execute main function
main "$@"

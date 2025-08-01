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
#  Usage:
#  Run this script directly without any arguments:
#      ./install_fix-permissions.sh
#      ./install_fix-permissions.sh --uninstall
#
#  Requirements:
#  - The `SCRIPTS` environment variable must be set to the directory
#    containing the `fix-permissions` script and its configurations.
#  - Must be executed with sufficient permissions to modify system
#    directories (typically as root or with sudo).
#  - Requires `logrotate` to be installed for log rotation setup.
#  - The script will not overwrite an existing fix-permissions cron job
#    if it already exists.
#
#  Notes:
#  - The script ensures that `/var/log/sysadmin` is created if it does
#    not exist and configures it securely.
#  - If a log rotation configuration for `fix-permissions` already exists,
#    it will not be overwritten.
#  - The `fix-permissions` script is deployed to `/etc/cron.daily` with
#    appropriate permissions.
#
#  Version History:
#  v2.0 2025-08-01
#       Add uninstall option to remove fix-permissions components except log file.
#  v1.9 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.8 2025-05-17
#       Add deployment of fix-permissions.conf to /etc/cron.config with secure permissions.
#  v1.7 2025-04-27
#       Add error checks for fix-permissions installer and unify log messages.
#  v1.6 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
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
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
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

# Check if required commands are available and executable
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

# Perform installation steps
install() {
    # Perform initial checks
    check_system
    check_commands sudo cp chmod chown mkdir touch logrotate
    check_scripts
    check_sudo

    # Create /var/log/sysadmin if needed
    if [ ! -d /var/log/sysadmin ]; then
        echo "[INFO] Creating /var/log/sysadmin directory."
        if ! sudo mkdir -p /var/log/sysadmin; then
            echo "[ERROR] Failed to create /var/log/sysadmin." >&2
            exit 1
        fi
    fi

    sudo chmod 750 /var/log/sysadmin
    sudo chown root:adm /var/log/sysadmin

    # Create fix-permissions log file if needed
    if [ ! -f /var/log/sysadmin/fix-permissions.log ]; then
        echo "[INFO] Creating fix-permissions log file."
        if ! sudo touch /var/log/sysadmin/fix-permissions.log; then
            echo "[ERROR] Failed to create fix-permissions.log." >&2
            exit 1
        fi
    else
        echo "[INFO] Log file already exists: /var/log/sysadmin/fix-permissions.log"
    fi

    sudo chmod 640 /var/log/sysadmin/fix-permissions.log
    sudo chown root:adm /var/log/sysadmin/fix-permissions.log

    # Deploy logrotate configuration
    if [ ! -f /etc/logrotate.d/fix-permissions ]; then
        echo "[INFO] Deploying logrotate configuration."
        if ! sudo cp "$SCRIPTS/cron/etc/logrotate.d/fix-permissions" /etc/logrotate.d/fix-permissions; then
            echo "[ERROR] Failed to deploy logrotate configuration." >&2
            exit 1
        fi
    else
        echo "[INFO] Logrotate config already exists: /etc/logrotate.d/fix-permissions."
    fi

    sudo chmod 640 /etc/logrotate.d/fix-permissions
    sudo chown root:adm /etc/logrotate.d/fix-permissions

    # Deploy the fix-permissions script
    echo "[INFO] Deploying fix-permissions cron job."
    if ! sudo cp "$SCRIPTS/cron/bin/fix-permissions.sh" /etc/cron.daily/fix-permissions; then
        echo "[ERROR] Failed to deploy fix-permissions cron job." >&2
        exit 1
    fi
    echo "[INFO] Cron job deployed: /etc/cron.daily/fix-permissions"

    sudo chmod 740 /etc/cron.daily/fix-permissions
    sudo chown root:adm /etc/cron.daily/fix-permissions

    # Create /etc/cron.config if needed
    if [ ! -d /etc/cron.config ]; then
        echo "[INFO] Creating /etc/cron.config directory."
        if ! sudo mkdir -p /etc/cron.config; then
            echo "[ERROR] Failed to create /etc/cron.config." >&2
            exit 1
        fi
    fi

    sudo chmod 750 /etc/cron.config
    sudo chown root:adm /etc/cron.config

    # Deploy fix-permissions.conf
    if [ ! -f /etc/cron.config/fix-permissions.conf ]; then
        echo "[INFO] Deploying fix-permissions.conf configuration file."
        if ! sudo cp "$SCRIPTS/cron/etc/fix-permissions.conf" /etc/cron.config/fix-permissions.conf; then
            echo "[ERROR] Failed to copy fix-permissions.conf." >&2
            exit 1
        fi
    else
        echo "[INFO] Configuration file already exists: /etc/cron.config/fix-permissions.conf."
        echo "[INFO] Skipping deployment to preserve existing configuration."
    fi

    sudo chmod 640 /etc/cron.config/fix-permissions.conf
    sudo chown root:adm /etc/cron.config/fix-permissions.conf

    echo "[INFO] Fix-permissions script setup completed successfully."
}

# Remove fix-permissions components except logs
uninstall() {
    check_commands sudo rm
    check_sudo

    echo "[INFO] Starting fix-permissions uninstallation..."

    for path in \
        /etc/cron.daily/fix-permissions \
        /etc/cron.config/fix-permissions.conf \
        /etc/logrotate.d/fix-permissions
    do
        if [ -e "$path" ]; then
            echo "[INFO] Removing $path"
            if ! sudo rm -f "$path"; then
                echo "[WARN] Failed to remove $path" >&2
            fi
        else
            echo "[INFO] $path not found. Skipping."
        fi
    done

    echo "[INFO] fix-permissions uninstallation completed."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version)
            usage
            ;;
        -u|--uninstall)
            uninstall
            ;;
        ""|*)
            install "$@"
            ;;
    esac

    return 0
}

# Execute main function
main "$@"
exit $?

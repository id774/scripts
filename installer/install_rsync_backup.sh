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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.6 2025-04-29
#       Use sudo for directory and file existence checks to ensure correct behavior under restricted permissions.
#       Correct the log filename from rsync_backup to rsync_backup.log for consistency.
#  v2.5 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v2.4 2025-04-11
#       Prevent overwriting existing rsync_backup.conf during deployment.
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v2.3 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v2.2 2025-03-14
#       Added command existence check, system check, and environment variable validation.
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
#  Usage:
#      ./install_rsync_backup.sh
#
#  Notes:
#  - Ensure the SCRIPTS environment variable is set to the directory containing
#    the rsync backup scripts and configuration files before running this script.
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
        echo "Please set the SCRIPTS variable to the directory containing the rsync backup related files." >&2
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
    check_commands sudo rsync cp chmod chown mkdir touch
    check_scripts
    check_sudo

    echo "[INFO] Starting rsync backup installation."
    # Make Directory if it doesn't exist and set permissions
    if ! sudo test -d /var/log/sysadmin; then
        echo "[INFO] Creating /var/log/sysadmin."
        sudo mkdir -p /var/log/sysadmin
        sudo chmod 750 /var/log/sysadmin
        sudo chown root:adm /var/log/sysadmin
    fi

    # Deploy rsync backup script and configuration
    echo "[INFO] Deploying rsync_backup.sh to /root/bin."
    sudo cp "$SCRIPTS/cron/bin/rsync_backup.sh" /root/bin/
    sudo chmod 700 /root/bin/rsync_backup.sh
    sudo chown root:root /root/bin/rsync_backup.sh

    CONFIG_FILE="/root/etc/rsync_backup.conf"

    echo "[INFO] Deploying rsync_backup.conf to /root/etc."
    if ! sudo test -f "$CONFIG_FILE"; then
        sudo cp "$SCRIPTS/cron/etc/rsync_backup.conf" "$CONFIG_FILE"
    else
        echo "[INFO] Configuration file already exists: $CONFIG_FILE"
        echo "[INFO] Skipping copy to preserve existing configuration."
    fi
    sudo chmod 600 "$CONFIG_FILE"
    sudo chown root:root "$CONFIG_FILE"

    # Deploy rsync backup cron job
    echo "[INFO] Installing cron job to /etc/cron.hourly."
    sudo cp "$SCRIPTS/cron/bin/rsync_backup" /etc/cron.hourly/
    sudo chmod 740 /etc/cron.hourly/rsync_backup
    sudo chown root:adm /etc/cron.hourly/rsync_backup

    # Set up rsync backup log file and permissions
    if ! sudo test -f /var/log/sysadmin/rsync_backup.log; then
        echo "[INFO] Creating rsync_backup log file."
        sudo touch /var/log/sysadmin/rsync_backup.log
        sudo chmod 640 /var/log/sysadmin/rsync_backup.log
        sudo chown root:adm /var/log/sysadmin/rsync_backup.log
    fi

    # Deploy log rotation configuration for rsync backup logs
    if ! sudo test -f /etc/logrotate.d/rsync_backup; then
        echo "[INFO] Installing logrotate configuration."
        sudo cp "$SCRIPTS/cron/etc/logrotate.d/rsync_backup" /etc/logrotate.d/
        sudo chmod 644 /etc/logrotate.d/rsync_backup
        sudo chown root:root /etc/logrotate.d/rsync_backup
    fi

    echo "[INFO] Rsync backup setup completed successfully."
}

# Execute main function
main "$@"

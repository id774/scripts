#!/bin/sh

########################################################################
# install_rsync_backup.sh: Rsync Backup Setup Script
#
#  Description:
#  This script sets up automated backups using rsync by deploying the
#  rsync_backup.sh script to /etc/cron.exec and its configuration to
#  /etc/cron.config, installing a cron trigger into /etc/cron.hourly for
#  scheduled execution, and configuring log rotation for backup logs.
#  It ensures that the necessary directories and log files exist, sets
#  appropriate permissions, and deploys all required files securely.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_rsync_backup.sh
#
#  Notes:
#  - Ensure the SCRIPTS environment variable is set to the directory
#    containing the rsync backup scripts and configuration files before
#    running this script.
#  - Deploys:
#      - rsync_backup.sh to /etc/cron.exec (main logic)
#      - rsync_backup to /etc/cron.hourly (cron trigger)
#      - rsync_backup.conf to /etc/cron.config (configuration file)
#      - logrotate config to /etc/logrotate.d/
#  - Sets appropriate ownership and permissions:
#      - All files owned by root:adm unless otherwise required
#      - Permissions: 740 for executables, 640 for configs and logs
#
#  Version History:
#  v2.8 2025-07-30
#       Move rsync_backup.sh to /etc/cron.exec, config to /etc/cron.config,
#       and cron trigger to /etc/cron.hourly. Adjust permissions and ownership.
#  v2.7 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
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

# Install script and related files.
install_rsync_backup() {
    echo "[INFO] Starting rsync backup installation."

    # Create /var/log/sysadmin if missing
    if ! sudo test -d /var/log/sysadmin; then
        echo "[INFO] Creating /var/log/sysadmin."
        sudo mkdir -p /var/log/sysadmin
    fi
    sudo chmod 750 /var/log/sysadmin
    sudo chown root:adm /var/log/sysadmin

    # Create /etc/cron.config if missing
    if ! sudo test -d /etc/cron.config; then
        echo "[INFO] Creating /etc/cron.config."
        sudo mkdir -p /etc/cron.config
    fi
    sudo chmod 750 /etc/cron.config
    sudo chown root:adm /etc/cron.config

    CONFIG_FILE="/etc/cron.config/rsync_backup.conf"

    echo "[INFO] Deploying rsync_backup.conf to /etc/cron.config."
    if ! sudo test -f "$CONFIG_FILE"; then
        sudo cp "$SCRIPTS/cron/etc/rsync_backup.conf" "$CONFIG_FILE"
    else
        echo "[INFO] Configuration file already exists: $CONFIG_FILE"
        echo "[INFO] Skipping copy to preserve existing configuration."
    fi
    sudo chmod 640 "$CONFIG_FILE"
    sudo chown root:adm "$CONFIG_FILE"

    # Create /etc/cron.exec if missing
    if ! sudo test -d /etc/cron.exec; then
        echo "[INFO] Creating /etc/cron.exec."
        sudo mkdir -p /etc/cron.exec
    fi
    sudo chmod 750 /etc/cron.exec
    sudo chown root:adm /etc/cron.exec

    # Deploy rsync_backup.sh script
    echo "[INFO] Deploying rsync_backup.sh to /etc/cron.exec."
    sudo cp "$SCRIPTS/cron/bin/rsync_backup.sh" /etc/cron.exec/
    sudo chmod 740 /etc/cron.exec/rsync_backup.sh
    sudo chown root:adm /etc/cron.exec/rsync_backup.sh

    # Deploy rsync backup cron job trigger
    echo "[INFO] Installing cron job trigger to /etc/cron.hourly."
    sudo cp "$SCRIPTS/cron/bin/rsync_backup" /etc/cron.hourly/
    sudo chmod 740 /etc/cron.hourly/rsync_backup
    sudo chown root:adm /etc/cron.hourly/rsync_backup

    # Set up rsync backup log file
    if ! sudo test -f /var/log/sysadmin/rsync_backup.log; then
        echo "[INFO] Creating rsync_backup log file."
        sudo touch /var/log/sysadmin/rsync_backup.log
    fi
    sudo chmod 640 /var/log/sysadmin/rsync_backup.log
    sudo chown root:adm /var/log/sysadmin/rsync_backup.log

    # Deploy logrotate configuration
    if ! sudo test -f /etc/logrotate.d/rsync_backup; then
        echo "[INFO] Installing logrotate configuration."
        sudo cp "$SCRIPTS/cron/etc/logrotate.d/rsync_backup" /etc/logrotate.d/
    fi
    sudo chmod 640 /etc/logrotate.d/rsync_backup
    sudo chown root:adm /etc/logrotate.d/rsync_backup
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Perform initial checks
    check_system
    check_commands sudo rsync cp chmod chown mkdir touch
    check_scripts
    check_sudo

    install_rsync_backup

    echo "[INFO] Rsync backup setup completed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

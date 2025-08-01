#!/bin/sh

########################################################################
# install_apache_log_analysis.sh: Setup Apache Log Analysis
#
#  Description:
#  This script sets up Apache log analysis by:
#  - Deploying analysis scripts and configurations.
#  - Installing scheduled cron job into /etc/cron.exec for log analysis.
#  - Ensuring proper file and directory permissions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script to set up Apache log analysis:
#      ./install_apache_log_analysis.sh
#      ./install_apache_log_analysis.sh --uninstall
#
#  Requirements:
#  - Must be executed with sudo privileges.
#  - The `$SCRIPTS` environment variable must be set.
#
#  Version History:
#  v2.0 2025-08-01
#       Move apache_log_analysis cron job from /etc/cron.daily to /etc/cron.exec.
#       Add uninstall option to remove configuration while retaining log file.
#  v1.9 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.8 2025-04-26
#       Add critical failure checks to script deployment steps.
#  v1.7 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v1.6 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.5 2025-04-11
#       Prevent overwriting existing configuration file during deployment.
#  v1.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
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
        echo "Please set the SCRIPTS variable to the directory containing Apache log analysis scripts." >&2
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

# Create required directories with proper permissions
setup_directories() {
    echo "[INFO] Setting up directories."
    if [ ! -d /var/log/sysadmin ]; then
        sudo mkdir -p /var/log/sysadmin
        sudo chmod 750 /var/log/sysadmin
        sudo chown root:adm /var/log/sysadmin
    fi
}

# Deploy Apache log analysis scripts
deploy_scripts() {
    echo "[INFO] Deploying log analysis scripts."
    if ! sudo mkdir -p /etc/cron.exec; then
        echo "[ERROR] Failed to create /etc/cron.exec." >&2
        exit 1
    fi

    sudo chmod 750 /etc/cron.exec
    sudo chown root:adm /etc/cron.exec

    for script in apache_log_analysis.sh apache_calculater.py; do
        if ! sudo cp "$SCRIPTS/$script" "/etc/cron.exec/$script"; then
            echo "[ERROR] Failed to copy $script to /etc/cron.exec." >&2
            exit 1
        fi

        sudo chmod 740 "/etc/cron.exec/$script"
        sudo chown root:adm "/etc/cron.exec/$script"
    done
}

# Deploy configuration files
deploy_configurations() {
    echo "[INFO] Deploying configuration files."

    CONFIG_DIR="/etc/cron.config"
    CONFIG_FILE="$CONFIG_DIR/apache_ignore.list"

    if ! sudo mkdir -p "$CONFIG_DIR"; then
        echo "[ERROR] Failed to create $CONFIG_DIR." >&2
        exit 1
    fi
    sudo chmod 750 "$CONFIG_DIR"
    sudo chown root:adm "$CONFIG_DIR"

    if ! sudo test -f "$CONFIG_FILE"; then
        if ! sudo cp "$SCRIPTS/etc/apache_ignore.list" "$CONFIG_FILE"; then
            echo "[ERROR] Failed to copy apache_ignore.list to $CONFIG_FILE." >&2
            exit 1
        fi
    else
        echo "[INFO] Configuration file already exists: $CONFIG_FILE"
        echo "[INFO] Skipping copy to preserve existing configuration."
    fi

    sudo chmod 640 "$CONFIG_FILE"
    sudo chown root:adm "$CONFIG_FILE"
}

# Deploy cron jobs
setup_cron_jobs() {
    echo "[INFO] Setting up cron jobs."
    if ! sudo cp "$SCRIPTS/cron/bin/apache_log_analysis" /etc/cron.daily/apache_log_analysis; then
        echo "[ERROR] Failed to deploy apache_log_analysis cron job." >&2
        exit 1
    fi

    if ! sudo mkdir -p /etc/cron.exec; then
        echo "[ERROR] Failed to create /etc/cron.exec." >&2
        exit 1
    fi

    sudo chmod 740 /etc/cron.daily/apache_log_analysis
    sudo chown root:adm /etc/cron.daily/apache_log_analysis
}

# Set up log files
setup_log_files() {
    echo "[INFO] Setting up log files."
    if [ ! -f /var/log/sysadmin/apache_summary.log ]; then
        sudo touch /var/log/sysadmin/apache_summary.log
        sudo chmod 640 /var/log/sysadmin/apache_summary.log
        sudo chown root:adm /var/log/sysadmin/apache_summary.log
    fi
}

# Deploy log rotation configuration
setup_log_rotation() {
    echo "[INFO] Deploying log rotation configuration."
    if ! sudo cp "$SCRIPTS/cron/etc/logrotate.d/apache_summary" /etc/logrotate.d/apache_summary; then
        echo "[ERROR] Failed to deploy logrotate configuration for apache_summary." >&2
        exit 1
    fi
    sudo chmod 644 /etc/logrotate.d/apache_summary
    sudo chown root:root /etc/logrotate.d/apache_summary
}

# Perform installation steps
install() {
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

    echo "[INFO] Apache log analysis setup completed successfully."
}

# Remove apache log analysis components except log files
uninstall() {
    check_commands sudo rm
    check_sudo

    echo "[INFO] Starting Apache log analysis uninstallation..."

    for path in \
        /etc/cron.daily/apache_log_analysis \
        /etc/cron.exec/apache_log_analysis.sh \
        /etc/cron.exec/apache_calculater.py \
        /etc/cron.config/apache_ignore.list \
        /etc/logrotate.d/apache_summary
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

    echo "[INFO] Apache log analysis uninstallation completed."
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

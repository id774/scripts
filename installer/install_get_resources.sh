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
#  Usage:
#      ./install_get_resources.sh
#
#  Notes:
#  - Ensure the SCRIPTS environment variable is set to the directory containing
#    the get_resources script and its related files before running this script.
#
#  Version History:
#  v2.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v2.4 2025-05-10
#       Always overwrite existing get_resources cron job to ensure consistency.
#  v2.3 2025-04-27
#       Skip deployment if get_resources.sh or its cron job already exists,
#       and add error handling for copy failures.
#  v2.2 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v2.1 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
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

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Perform initial checks
    check_system
    check_commands sudo cp chmod chown mkdir touch
    check_scripts
    check_sudo

    echo "[INFO] Starting server resource report setup."

    if [ ! -d /var/log/sysadmin ]; then
        echo "[INFO] Creating /var/log/sysadmin directory."
        if ! sudo mkdir -p /var/log/sysadmin; then
            echo "[ERROR] Failed to create /var/log/sysadmin." >&2
            exit 1
        fi
    fi
    sudo chmod 750 /var/log/sysadmin
    sudo chown root:adm /var/log/sysadmin

    if [ ! -f /var/log/sysadmin/resources.log ]; then
        echo "[INFO] Creating resources.log file."
        if ! sudo touch /var/log/sysadmin/resources.log; then
            echo "[ERROR] Failed to create /var/log/sysadmin/resources.log." >&2
            exit 1
        fi
    fi
    sudo chmod 640 /var/log/sysadmin/resources.log
    sudo chown root:adm /var/log/sysadmin/resources.log

    if [ ! -f /etc/logrotate.d/resources ]; then
        echo "[INFO] Deploying logrotate configuration."
        if ! sudo cp "$SCRIPTS/cron/etc/logrotate.d/resources" /etc/logrotate.d/resources; then
            echo "[ERROR] Failed to copy logrotate configuration." >&2
            exit 1
        fi
    else
        echo "[INFO] /etc/logrotate.d/resources already exists. Skipping deployment."
    fi
    sudo chmod 640 /etc/logrotate.d/resources
    sudo chown root:adm /etc/logrotate.d/resources

    echo "[INFO] Deploying get_resources.sh to /root/bin."
    if [ -f /root/bin/get_resources.sh ]; then
        echo "[INFO] /root/bin/get_resources.sh already exists. Skipping deployment."
    else
        if ! sudo cp "$SCRIPTS/get_resources.sh" /root/bin/; then
            echo "[ERROR] Failed to copy get_resources.sh to /root/bin/." >&2
            exit 1
        fi
    fi
    sudo chmod 700 /root/bin/get_resources.sh
    sudo chown root:root /root/bin/get_resources.sh

    echo "[INFO] Installing cron job to /etc/cron.hourly."
    if ! sudo cp "$SCRIPTS/cron/bin/get_resources" /etc/cron.hourly/; then
        echo "[ERROR] Failed to copy cron job to /etc/cron.hourly/." >&2
        exit 1
    fi
    sudo chmod 740 /etc/cron.hourly/get_resources
    sudo chown root:adm /etc/cron.hourly/get_resources

    echo "[INFO] Server resource report setup completed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

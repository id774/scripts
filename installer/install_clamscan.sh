#!/bin/sh

########################################################################
# install_clamscan.sh: ClamAV AutoScan Setup Script
#
#  Description:
#  This script automates the setup for ClamAV scans by:
#  - Deploying the clamscan.sh script.
#  - Configuring clamscan exclusions.
#  - Setting up cron jobs specifically for weekend scanning in /etc/cron.weekend.
#  - Managing log rotation for ClamAV logs.
#  - If the clamscan cron job already exists in /etc/cron.weekend, the deployment is skipped automatically.
#  - Ensuring the necessary directories and log files exist, including creating /etc/cron.weekend if it doesn't exist.
#  - Setting appropriate permissions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Ensure the SCRIPTS environment variable is set before running:
#      export SCRIPTS=/path/to/scripts
#      ./install_clamscan.sh
#
#  Requirements:
#  - The user must have `sudo` installed.
#  - This script is intended for Linux systems only.
#
#  Version History:
#  v2.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v2.4 2025-05-11
#       Always overwrite clamscan cron job in /etc/cron.weekend during installation.
#  v2.3 2025-04-27
#       Add error checking for deployment steps and improve logrotate configuration handling.
#  v2.2 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v2.1 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v2.0 2025-04-05
#       Changed cron job setup from /etc/cron.weekly to /etc/cron.weekend for ClamAV scans.
#  v1.9 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.8 2025-03-15
#       Unified structure, added system checks, improved error handling.
#  v1.7 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.6 2024-03-17
#       Refactored script for improved readability and maintainability.
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
        echo "[ERROR] SCRIPTS environment variable is not set. Please set it to the directory containing ClamAV related files." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Deploy ClamAV setup files
install_clamscan() {
    echo "[INFO] Setting up ClamAV AutoScan environment..."

    echo "[INFO] Creating /var/log/sysadmin directory."
    sudo mkdir -p /var/log/sysadmin
    sudo chmod 750 /var/log/sysadmin
    sudo chown root:adm /var/log/sysadmin

    echo "[INFO] Deploying clamscan.sh to /root/bin."
    if ! sudo cp "$SCRIPTS/cron/bin/clamscan.sh" /root/bin/; then
        echo "[ERROR] Failed to copy clamscan.sh." >&2
        exit 1
    fi
    sudo chmod 700 /root/bin/clamscan.sh
    sudo chown root:root /root/bin/clamscan.sh

    echo "[INFO] Deploying clamscan_exclude to /root/etc."
    if ! sudo cp "$SCRIPTS/cron/etc/clamscan_exclude" /root/etc/; then
        echo "[ERROR] Failed to copy clamscan_exclude." >&2
        exit 1
    fi
    sudo rm -vf /root/bin/clamscan_exclude
    sudo chmod 600 /root/etc/clamscan_exclude
    sudo chown root:root /root/etc/clamscan_exclude

    echo "[INFO] Setting up /etc/cron.weekend directory."
    sudo mkdir -p /etc/cron.weekend

    echo "[INFO] Deploying clamscan cron job to /etc/cron.weekend."
    if ! sudo cp "$SCRIPTS/cron/bin/clamscan" /etc/cron.weekend/; then
        echo "[ERROR] Failed to copy clamscan cron job" >&2
        exit 1
    fi
    sudo chmod 740 /etc/cron.weekend/clamscan
    sudo chown root:adm /etc/cron.weekend/clamscan

    echo "[INFO] Setting up ClamAV log files."
    for log_file in /var/log/clamav/clamscan.log /var/log/clamav/clamav.log; do
        if [ ! -f "$log_file" ]; then
            echo "[INFO] Creating log file: $log_file"
            sudo touch "$log_file"
        fi
        sudo chmod 640 "$log_file"
        sudo chown clamav:adm "$log_file"
    done

    echo "[INFO] Deploying logrotate configuration for ClamAV."
    if [ -f /etc/logrotate.d/clamscan ]; then
        echo "[INFO] Skipping logrotate configuration: /etc/logrotate.d/clamscan already exists."
    else
        if ! sudo cp "$SCRIPTS/cron/etc/logrotate.d/clamscan" /etc/logrotate.d/; then
            echo "[ERROR] Failed to copy logrotate configuration." >&2
            exit 1
        fi
        sudo chmod 640 /etc/logrotate.d/clamscan
        sudo chown root:adm /etc/logrotate.d/clamscan
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands sudo cp rm chmod chown mkdir touch
    check_scripts
    check_sudo
    install_clamscan

    echo "[INFO] ClamAV AutoScan setup completed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

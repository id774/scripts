#!/bin/sh

########################################################################
# install_clamscan.sh: ClamAV AutoScan Setup Script
#
#  Description:
#  This script automates the setup for ClamAV scans by:
#  - Deploying the clamscan.sh script.
#  - Configuring clamscan exclusions.
#  - Setting up cron jobs for weekend scanning.
#  - Managing log rotation for ClamAV logs.
#  - Ensuring the necessary directories and log files exist.
#  - Setting appropriate permissions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.8 2025-03-15
#       Unified structure, added system checks, improved error handling.
#  v1.7 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.6 2024-03-17
#       Refactored script for improved readability and maintainability.
#  [Further version history truncated for brevity]
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

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set. Please set it to the directory containing ClamAV related files." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Deploy ClamAV setup files
install_clamscan() {
    # Ensure required directories exist
    sudo mkdir -p /var/log/sysadmin
    sudo chmod 750 /var/log/sysadmin
    sudo chown root:adm /var/log/sysadmin

    # Deploy clamscan script and exclusion file
    sudo cp "$SCRIPTS/cron/bin/clamscan.sh" /root/bin/
    sudo chmod 700 /root/bin/clamscan.sh
    sudo chown root:root /root/bin/clamscan.sh

    sudo cp "$SCRIPTS/cron/etc/clamscan_exclude" /root/etc/
    sudo rm -vf /root/bin/clamscan_exclude
    sudo chmod 600 /root/etc/clamscan_exclude
    sudo chown root:root /root/etc/clamscan_exclude

    # Deploy clamscan cron job
    sudo cp "$SCRIPTS/cron/bin/clamscan" /etc/cron.weekly/
    sudo chmod 740 /etc/cron.weekly/clamscan
    sudo chown root:adm /etc/cron.weekly/clamscan

    # Set up ClamAV log files and permissions
    for log_file in /var/log/clamav/clamscan.log /var/log/clamav/clamav.log; do
        [ -f "$log_file" ] || sudo touch "$log_file"
        sudo chmod 640 "$log_file"
        sudo chown clamav:adm "$log_file"
    done

    # Deploy log rotation configuration
    [ -f /etc/logrotate.d/clamscan ] || sudo cp "$SCRIPTS/cron/etc/logrotate.d/clamscan" /etc/logrotate.d/
    sudo chmod 640 /etc/logrotate.d/clamscan
    sudo chown root:adm /etc/logrotate.d/clamscan
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_commands sudo cp rm chmod chown mkdir touch
    check_scripts
    check_sudo
    install_clamscan
    echo "ClamAV AutoScan setup completed successfully."
}

# Execute main function
main "$@"

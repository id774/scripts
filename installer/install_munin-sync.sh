#!/bin/sh

########################################################################
# install_munin-sync.sh: Munin Sync Installation Script
#
#  Description:
#  This script installs the munin-sync system by setting up necessary
#  directories, deploying scripts and configurations, and configuring
#  cron jobs for periodic execution. It ensures the correct permissions
#  are set for all files and directories involved.
#
#  This script is designed to run on Linux systems and requires sudo
#  privileges to modify system directories and files.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-04-10
#       Do not overwrite existing configuration file.
#  v1.0 2025-04-07
#       First version.
#
#  Usage:
#      ./install_munin-sync.sh
#
#  Functions:
#  - setup_directories: Creates necessary directories for storing scripts,
#    configurations, and log files with appropriate permissions.
#  - deploy_scripts: Copies the munin-sync.sh script to the appropriate
#    location and sets the correct ownership and permissions.
#  - deploy_configurations: Copies the munin-sync.conf configuration file
#    to the appropriate location and ensures correct permissions.
#  - setup_cron_jobs: Configures cron jobs to periodically execute the
#    munin-sync.sh script.
#
#  Notes:
#  - This script must be executed on a Linux system with sudo privileges.
#  - The SCRIPTS environment variable must be set to the directory containing the munin-sync source files.
#  - The script assumes that the Munin service is installed and that /var/lib/munin exists.
#  - The configuration file (/var/lib/munin/etc/munin-sync.conf) will not be overwritten if it already exists.
#    Please edit it manually if changes are needed after initial deployment.
#  - The munin-sync.sh script is installed under /var/lib/munin/bin and owned by the 'munin' user.
#  - Cron jobs are configured under /etc/cron.d/ and run every 5 minutes as the 'munin' user.
#  - Log and configuration directories are created with restricted permissions for security.
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

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
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

# Check if /var/lib/munin exists
check_munin_dir() {
    if [ ! -d "/var/lib/munin" ]; then
        echo "[ERROR] /var/lib/munin directory does not exist. Please ensure Munin is installed properly." >&2
        exit 1
    fi
}

# Setup necessary directories
setup_directories() {
    echo "[INFO] Setting up directories..."
    sudo mkdir -p /var/lib/munin/bin
    sudo chmod 750 /var/lib/munin/bin
    sudo chown munin:munin /var/lib/munin/bin

    sudo mkdir -p /var/lib/munin/etc
    sudo chmod 750 /var/lib/munin/etc
    sudo chown munin:munin /var/lib/munin/etc

    SENDING_DIR="/var/lib/munin/sending/$(hostname)"
    sudo mkdir -p "$SENDING_DIR"
    sudo chmod 750 /var/lib/munin/sending
    sudo chmod 750 "$SENDING_DIR"
    sudo chown munin:munin /var/lib/munin/sending
    sudo chown munin:munin "$SENDING_DIR"
}

# Deploy the munin-sync script
deploy_scripts() {
    echo "[INFO] Deploying scripts..."
    sudo cp "$SCRIPTS/cron/bin/munin-sync.sh" /var/lib/munin/bin/
    sudo chmod 750 /var/lib/munin/bin/munin-sync.sh
    sudo chown munin:munin /var/lib/munin/bin/munin-sync.sh
}

# Deploy the configuration file
deploy_configurations() {
    echo "[INFO] Deploying configurations..."
    CONFIG_FILE="/var/lib/munin/etc/munin-sync.conf"

    if ! sudo test -f "$CONFIG_FILE"; then
        sudo cp "$SCRIPTS/cron/etc/munin-sync.conf" "$CONFIG_FILE"
    else
        echo "[INFO] Configuration file already exists: $CONFIG_FILE"
        echo "[INFO] Skipping copy to preserve existing configuration."
    fi

    sudo chmod 640 "$CONFIG_FILE"
    sudo chown munin:munin "$CONFIG_FILE"
}

# Setup cron jobs for munin-sync
setup_cron_jobs() {
    echo "[INFO] Setting up cron jobs..."
    CRON_FILE="/etc/cron.d/munin-sync"

    if ! sudo test -f "$CRON_FILE"; then
        sudo tee "$CRON_FILE" > /dev/null <<EOF
2-57/5 * * * * munin test -x /var/lib/munin/bin/munin-sync.sh && /var/lib/munin/bin/munin-sync.sh
EOF
    else
        echo "[INFO] Cron job already exists: $CRON_FILE"
        echo "[INFO] Skipping creation to preserve existing configuration."
    fi
    sudo chmod 644 "$CRON_FILE"
    sudo chown root:root "$CRON_FILE"
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_munin_dir
    check_commands sudo cp chmod chown mkdir touch tee rsync hostname grep
    check_scripts
    check_sudo

    setup_directories
    deploy_scripts
    deploy_configurations
    setup_cron_jobs

    echo "[INFO] Installation completed successfully."
}

# Execute main function
main "$@"

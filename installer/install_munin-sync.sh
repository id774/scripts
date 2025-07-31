#!/bin/sh

########################################################################
# install_munin-sync.sh: Munin Sync Installation Script
#
#  Description:
#  This script installs or uninstalls the munin-sync system by setting up
#  or removing necessary directories, deploying or deleting scripts and
#  configurations, and configuring or removing cron jobs for periodic execution.
#  It ensures the correct permissions are set and removed cleanly.
#
#  This script is designed to run on Linux systems and requires sudo
#  privileges to modify system directories and files.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_munin-sync.sh               # Install mode
#      ./install_munin-sync.sh --uninstall   # Uninstall mode
#      ./install_munin-sync.sh --help        # Show script help
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
#  - uninstall_munin_sync: Removes all files and directories created by
#    this script including cron job, scripts, configs, and sending data.
#
#  Notes:
#  - This script must be executed on a Linux system with sudo privileges.
#  - The SCRIPTS environment variable must be set to the directory containing the munin-sync source files (install only).
#  - The script assumes that the Munin service is installed and that /var/lib/munin exists.
#  - The configuration file (/var/lib/munin/etc/munin-sync.conf) will not be overwritten if it already exists.
#    Please edit it manually if changes are needed after initial deployment.
#  - The munin-sync.sh script is installed under /var/lib/munin/bin and owned by the 'munin' user.
#  - Cron jobs are configured under /etc/cron.d/ and run every 5 minutes as the 'munin' user.
#  - Log and configuration directories are created with restricted permissions for security.
#  - The --uninstall option will remove the following:
#       /var/lib/munin/bin/
#       /var/lib/munin/etc/
#       /var/lib/munin/sending/
#       /etc/cron.d/munin-sync
#
#  Version History:
#  v1.4 2025-07-31
#       Add --uninstall option to remove all installed components.
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-27
#       Add strict error checking for deployment steps.
#  v1.1 2025-04-10
#       Do not overwrite existing configuration file.
#  v1.0 2025-04-07
#       First version.
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
    if ! sudo mkdir -p /var/lib/munin/bin; then
        echo "[ERROR] Failed to create /var/lib/munin/bin." >&2
        exit 1
    fi
    sudo chmod 750 /var/lib/munin/bin
    sudo chown munin:munin /var/lib/munin/bin

    if ! sudo mkdir -p /var/lib/munin/etc; then
        echo "[ERROR] Failed to create /var/lib/munin/etc." >&2
        exit 1
    fi
    sudo chmod 750 /var/lib/munin/etc
    sudo chown munin:munin /var/lib/munin/etc

    SENDING_DIR="/var/lib/munin/sending/$(hostname)"
    if ! sudo mkdir -p "$SENDING_DIR"; then
        echo "[ERROR] Failed to create $SENDING_DIR." >&2
        exit 1
    fi
    sudo chmod 750 /var/lib/munin/sending
    sudo chmod 750 "$SENDING_DIR"
    sudo chown munin:munin /var/lib/munin/sending
    sudo chown munin:munin "$SENDING_DIR"
}

# Deploy the munin-sync script
deploy_scripts() {
    echo "[INFO] Deploying scripts..."
    if ! sudo cp "$SCRIPTS/cron/bin/munin-sync.sh" /var/lib/munin/bin/; then
        echo "[ERROR] Failed to copy munin-sync.sh." >&2
        exit 1
    fi
    sudo chmod 750 /var/lib/munin/bin/munin-sync.sh
    sudo chown munin:munin /var/lib/munin/bin/munin-sync.sh
}

# Deploy the configuration file
deploy_configurations() {
    echo "[INFO] Deploying configurations..."
    CONFIG_FILE="/var/lib/munin/etc/munin-sync.conf"

    if ! sudo test -f "$CONFIG_FILE"; then
        if ! sudo cp "$SCRIPTS/cron/etc/munin-sync.conf" "$CONFIG_FILE"; then
            echo "[ERROR] Failed to copy configuration file." >&2
            exit 1
        fi
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
        if ! sudo tee "$CRON_FILE" > /dev/null <<EOF
1-56/5 * * * * munin test -x /var/lib/munin/bin/munin-sync.sh && /var/lib/munin/bin/munin-sync.sh
EOF
        then
            echo "[ERROR] Failed to create cron job file." >&2
            exit 1
        fi
    else
        echo "[INFO] Cron job already exists: $CRON_FILE"
        echo "[INFO] Skipping creation to preserve existing configuration."
    fi
    sudo chmod 644 "$CRON_FILE"
    sudo chown root:root "$CRON_FILE"
}

# Uninstall munin-sync components
uninstall() {
    check_commands rm
    check_sudo

    echo "[INFO] Uninstalling munin-sync..."

    for path in \
        /var/lib/munin/bin \
        /var/lib/munin/etc \
        /var/lib/munin/sending \
        /etc/cron.d/munin-sync; do
        if sudo test -e "$path"; then
            echo "[INFO] Removing $path"
            sudo rm -rf "$path" || {
                echo "[ERROR] Failed to remove $path" >&2
                exit 1
            }
        else
            echo "[INFO] $path not found, skipping"
        fi
    done

    echo "[INFO] Uninstallation completed successfully."
}

# Perform installation steps
install() {
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

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version)
            usage
            ;;
        --uninstall)
            uninstall
            ;;
        *)
            install
            ;;
    esac
    return 0
}

# Execute main function
main "$@"
exit $?

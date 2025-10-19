#!/bin/sh

########################################################################
# install_munin-symlink.sh: Munin Symlink Monitor Installation Script
#
#  Description:
#  This script installs the munin-symlink system by setting up necessary
#  directories, deploying the monitor script and its configuration, and
#  configuring a cron job for periodic execution. It ensures correct
#  permissions are set for all deployed files.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_munin-symlink.sh
#      ./install_munin-symlink.sh --uninstall
#      This installs /etc/cron.exec/munin-symlink.sh and /etc/cron.config/munin-symlink.conf
#
#  Notes:
#  - This script must be executed on a Linux system with root privileges (via sudo).
#  - The environment variable SCRIPTS must be set to the root of the script repository.
#  - The deployed configuration file (/etc/cron.config/munin-symlink.conf) will not be overwritten if it already exists.
#    Please edit it manually if configuration changes are needed after deployment.
#  - The deployed monitor script (/etc/cron.exec/munin-symlink.sh) runs every 5 minutes via a cron job.
#    Ensure that Munin is properly configured to read its output, typically via symlink checks.
#  - Permissions and ownership of deployed files are strictly set to restrict access to root only.
#  - The monitor script is deployed with permission 740 (readable by adm group).
#  - The configuration file is deployed with permission 640 (readable by adm group).
#
#  Version History:
#  v2.1 2025-09-24
#       Fix group ownership and command checks.
#  v2.0 2025-08-01
#       Add --uninstall option to remove installed script, config, and cron job.
#  v1.4 2025-07-30
#       Move script and config to /etc/cron.exec and /etc/cron.config.
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-27
#       Add strict error checking for deployment steps.
#  v1.1 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.0 2025-04-10
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
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if required commands are available
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

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Deploy the monitor script
deploy_script() {
    echo "[INFO] Deploying script..."
    if ! sudo mkdir -p /etc/cron.exec; then
        echo "[ERROR] Failed to create /etc/cron.exec." >&2
        exit 1
    fi

    sudo chmod 0750 /etc/cron.exec
    sudo chown root:adm /etc/cron.exec

    if ! sudo cp "$SCRIPTS/cron/bin/munin-symlink.sh" /etc/cron.exec/munin-symlink.sh; then
        echo "[ERROR] Failed to copy munin-symlink.sh." >&2
        exit 1
    fi
    if ! sudo chown root:adm /etc/cron.exec/munin-symlink.sh; then
        echo "[ERROR] Failed to change ownership of munin-symlink.sh." >&2
        exit 1
    fi
    if ! sudo chmod 0740 /etc/cron.exec/munin-symlink.sh; then
        echo "[ERROR] Failed to set permissions on munin-symlink.sh." >&2
        exit 1
    fi
}

# Deploy the configuration file
deploy_configuration() {
    echo "[INFO] Deploying configuration..."
    CONFIG_DIR="/etc/cron.config"
    CONFIG_FILE="/etc/cron.config/munin-symlink.conf"

    if ! sudo mkdir -p "$CONFIG_DIR"; then
        echo "[ERROR] Failed to create $CONFIG_DIR." >&2
        exit 1
    fi
    sudo chmod 0750 "$CONFIG_DIR"
    sudo chown root:adm "$CONFIG_DIR"

    if ! sudo test -f "$CONFIG_FILE"; then
        if ! sudo cp "$SCRIPTS/cron/etc/munin-symlink.conf" "$CONFIG_FILE"; then
            echo "[ERROR] Failed to copy configuration file." >&2
            exit 1
        fi
    else
        echo "[INFO] Configuration file already exists: $CONFIG_FILE"
        echo "[INFO] Skipping copy to preserve existing configuration."
    fi

    if ! sudo chmod 0640 "$CONFIG_FILE"; then
        echo "[ERROR] Failed to set permissions on configuration file." >&2
        exit 1
    fi
    if ! sudo chown root:adm "$CONFIG_FILE"; then
        echo "[ERROR] Failed to change ownership of configuration file." >&2
        exit 1
    fi
}

# Setup cron jobs for munin-symlink
setup_cron_job() {
    echo "[INFO] Setting up cron job..."
    CRON_FILE="/etc/cron.d/munin-symlink"

    if ! sudo test -f "$CRON_FILE"; then
        if ! sudo tee "$CRON_FILE" > /dev/null <<EOF
*/5 * * * * root test -x /etc/cron.exec/munin-symlink.sh && /etc/cron.exec/munin-symlink.sh
EOF
        then
            echo "[ERROR] Failed to create cron job file." >&2
            exit 1
        fi
    else
        echo "[INFO] Cron job already exists: $CRON_FILE"
        echo "[INFO] Skipping creation to preserve existing configuration."
    fi

    sudo chmod 0640 "$CRON_FILE"
    sudo chown root:adm "$CRON_FILE"
}

# Perform installation steps
install() {
    check_system
    check_commands sudo chmod chown tee mkdir cp
    check_scripts
    check_sudo

    deploy_script
    deploy_configuration
    setup_cron_job

    echo "[INFO] Installation completed successfully."
}

# Uninstall all installed components
uninstall() {
    check_commands sudo rm
    check_sudo

    echo "[INFO] Uninstalling munin-symlink..."

    FILES_TO_REMOVE="
        /etc/cron.exec/munin-symlink.sh
        /etc/cron.config/munin-symlink.conf
        /etc/cron.d/munin-symlink
    "

    for file in $FILES_TO_REMOVE; do
        if sudo test -e "$file"; then
            if sudo rm -f "$file"; then
                echo "[INFO] Removed $file"
            else
                echo "[ERROR] Failed to remove $file" >&2
                exit 1
            fi
        else
            echo "[INFO] File not found, skipping: $file"
        fi
    done

    echo "[INFO] Uninstallation completed."
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
        *)
            install
            ;;
    esac
    return 0
}

# Execute main function
main "$@"

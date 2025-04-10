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
#  Version History:
#  v1.0 2025-04-10
#       First version.
#
#  Usage:
#      ./install_munin-symlink.sh
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
        echo "Error: This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if required commands are available
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

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set." >&2
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

# Deploy the monitor script
deploy_script() {
    echo "Deploying script..."
    sudo install -m 750 -o root -g root "$SCRIPTS/cron/bin/munin-symlink.sh" /root/bin/munin-symlink.sh
}

# Deploy the configuration file
deploy_configuration() {
    echo "Deploying configuration..."
    CONFIG_FILE="/root/etc/munin-symlink.conf"
    if [ ! -f "$CONFIG_FILE" ]; then
        sudo install -m 640 -o root -g root "$SCRIPTS/cron/etc/munin-symlink.conf" "$CONFIG_FILE"
    else
        echo "Configuration file already exists: $CONFIG_FILE"
        echo "Skipping copy to preserve existing configuration."
    fi
    sudo chmod 640 "$CONFIG_FILE"
    sudo chown root:root "$CONFIG_FILE"
}

# Setup cron jobs for munin-symlink
setup_cron_job() {
    echo "Setting up cron job..."
    CRON_FILE="/etc/cron.d/munin-symlink"
    sudo tee "$CRON_FILE" > /dev/null <<EOF
*/5 * * * * root test -x /root/bin/munin-symlink.sh && /root/bin/munin-symlink.sh
EOF
    sudo chmod 644 "$CRON_FILE"
    sudo chown root:root "$CRON_FILE"
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_commands sudo install chmod chown tee test
    check_scripts
    check_sudo

    deploy_script
    deploy_configuration
    setup_cron_job

    echo "Installation completed successfully."
}

# Execute main function
main "$@"

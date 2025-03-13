#!/bin/sh

########################################################################
# install_munin.sh: Install and configure Munin monitoring
#
#  Description:
#  This script installs and configures Munin and Munin-node,
#  ensuring necessary permissions and configurations are applied
#  automatically without requiring manual edits.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-13
#       Added system compatibility check for Linux.
#       Removed manual vi editing and automated configuration steps.
#  [Further version history truncated for brevity]
#  v0.1 2011-07-07
#       First.
#
#  Usage:
#  Run the script directly:
#      ./install_munin.sh
#  This script installs Munin, configures it, and restarts necessary services.
#
#  Notes:
#  - The script is designed for Debian-based systems.
#  - Internet connectivity is required for package installation.
#  - Ensure that $SCRIPTS is set correctly before execution.
#
#  Error Conditions:
#  - If the system is not Linux, the script exits with an error.
#  - If required commands are missing, the script exits with an error.
#  - Errors from underlying commands should be resolved based on their output.
#
########################################################################

# Check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "Error: This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to verify script environment
setup_environment() {
    SCRIPTS="$HOME/scripts"
    if [ ! -d "$SCRIPTS" ]; then
        echo "Error: Directory '$SCRIPTS' does not exist. Please create it or specify the correct path." >&2
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Install Munin and dependencies
install_munin() {
    echo "Installing Munin and dependencies..."
    sudo apt-get -y install munin munin-node
}

# Configure Munin
configure_munin() {
    echo "Configuring Munin..."
    sudo cp "$SCRIPTS/etc/munin-apache.conf" /etc/munin/apache.conf
    sudo chown root:root /etc/munin/apache.conf
    test -f /etc/munin/apache24.conf && sudo rm -f /etc/munin/apache24.conf && sudo ln -s /etc/munin/apache.conf /etc/munin/apache24.conf
}

# Configure authentication
configure_authentication() {
    echo "Configuring Munin authentication..."
    if [ ! -r /etc/apache2/.htpasswd ]; then
        echo "Creating new Munin admin user..."
        sudo htpasswd -cb /etc/apache2/.htpasswd admin adminpassword
    fi
    sudo chown root:www-data /etc/apache2/.htpasswd
    sudo chmod 640 /etc/apache2/.htpasswd
}

# Restart services
restart_services() {
    echo "Restarting services..."
    sudo systemctl restart rsyslog
    sudo systemctl restart munin-node
    sudo systemctl restart apache2
}

# Main operation function
main() {
    check_system
    setup_environment
    check_commands sudo apt-get cp chown chmod htpasswd systemctl
    install_munin
    configure_munin
    configure_authentication
    restart_services
    "$SCRIPTS/munin_plugins_links.sh" -c
    echo "Munin installation and configuration completed successfully."
}

# Execute main operations
main "$@"

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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-03-26
#       Improve robustness with verbose and forced symbolic link operations in Apache config.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-13
#       Added system compatibility check for Linux.
#       Removed manual vi editing and automated configuration steps.
#       Redirected error messages to stderr for better logging and debugging.
#  [Further version history truncated for brevity]
#  v0.1 2011-07-07
#       First.
#
#  Usage:
#  Run the script directly:
#      ./install_munin.sh
#
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
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the install and configure Munin monitoring script." >&2
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

# Install Munin and dependencies
install_munin() {
    echo "Installing Munin and dependencies..."
    sudo apt-get -y install munin munin-node
}

# Configure Munin
configure_munin() {
    echo "Configuring Munin..."
    sudo cp -v "$SCRIPTS/etc/munin-apache.conf" /etc/munin/apache.conf
    sudo chown root:root /etc/munin/apache.conf
    test -f /etc/munin/apache24.conf && sudo rm -vf /etc/munin/apache24.conf && sudo ln -snf /etc/munin/apache.conf /etc/munin/apache24.conf
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

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    # Perform initial checks
    check_system
    check_scripts
    check_commands sudo systemctl apt-get htpasswd cp chown chmod

    # Run the installation process
    install_munin
    configure_munin
    configure_authentication
    restart_services
    "$SCRIPTS/installer/munin_plugins_links.sh" -c
    echo "Munin installation and configuration completed successfully."
}

# Execute main function
main "$@"

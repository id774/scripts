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
#  Version History:
#  v1.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.4 2025-04-26
#       Exit immediately on critical failures and
#       ensure success message is shown only if all steps succeed.
#  v1.3 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
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

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the install and configure Munin monitoring script." >&2
        exit 1
    fi
}

# Check required commands
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
    echo "[INFO] Installing Munin and dependencies..."
    if ! sudo apt-get -y install munin munin-node; then
        echo "[ERROR] Failed to install Munin packages." >&2
        exit 1
    fi
}

# Configure Munin
configure_munin() {
    echo "[INFO] Configuring Munin..."
    if ! sudo cp -v "$SCRIPTS/etc/munin-apache.conf" /etc/munin/apache.conf; then
        echo "[ERROR] Failed to copy apache.conf for Munin." >&2
        exit 1
    fi
    sudo chown root:root /etc/munin/apache.conf
    test -f /etc/munin/apache24.conf && sudo rm -vf /etc/munin/apache24.conf && sudo ln -snf /etc/munin/apache.conf /etc/munin/apache24.conf
}

# Configure authentication
configure_authentication() {
    echo "[INFO] Configuring Munin authentication..."
    if [ ! -r /etc/apache2/.htpasswd ]; then
        echo "[INFO] Creating new Munin admin user..."
        if ! sudo htpasswd -cb /etc/apache2/.htpasswd admin adminpassword; then
            echo "[ERROR] Failed to create .htpasswd for Munin." >&2
            exit 1
        fi
    fi
    sudo chown root:www-data /etc/apache2/.htpasswd
    sudo chmod 640 /etc/apache2/.htpasswd
}

# Restart services
restart_services() {
    echo "[INFO] Restarting services..."
    if ! sudo systemctl restart munin-node; then
        echo "[ERROR] Failed to restart munin-node." >&2
        exit 1
    fi
    if ! sudo systemctl restart apache2; then
        echo "[ERROR] Failed to restart apache2." >&2
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
    check_scripts
    check_commands sudo systemctl apt-get htpasswd cp chown chmod

    # Run the installation process
    install_munin
    configure_munin
    configure_authentication
    restart_services
    "$SCRIPTS/installer/munin_plugins_links.sh" -c

    echo "[INFO] Munin installation and configuration completed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

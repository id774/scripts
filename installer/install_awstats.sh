#!/bin/sh

########################################################################
# install_awstats.sh: Installer for AWStats with Apache Integration
#
#  Description:
#  This script automates the installation and configuration of AWStats
#  for Apache log analysis. It:
#  - Installs AWStats using apt-get.
#  - Configures AWStats and related Apache settings.
#  - Ensures proper permissions on log files.
#  - Restarts Apache and updates AWStats statistics.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-15
#       Added system, command, and sudo checks.
#       Improved error handling and permission settings.
#       Ensured idempotent execution.
#  v0.1 2011-09-07
#       Initial version.
#
#  Usage:
#  Run this script to install and configure AWStats:
#      ./install_awstats.sh
#
#  Requirements:
#  - Must be executed with sudo privileges.
#  - Apache2 must be installed on the system.
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


set -e  # Exit immediately on error

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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Install AWStats package
install_awstats() {
    echo "Installing AWStats..."
    sudo apt-get update
    sudo apt-get install -y awstats
}

# Configure AWStats
configure_awstats() {
    echo "Configuring AWStats and Apache..."

    # Ensure configuration files exist before opening with vi
    for file in /etc/awstats/awstats.conf* /etc/apache2/sites-available/custom* /etc/logrotate.d/apache2; do
        if [ ! -f "$file" ]; then
            echo "Warning: File $file does not exist. Skipping edit."
        else
            sudo vi "$file"
        fi
    done

    # Set correct permissions on log files
    echo "Setting permissions on Apache logs..."
    sudo chmod 440 /var/log/apache2/*
    sudo chown www-data:adm /var/log/apache2/*
    sudo chmod 550 /var/log/apache2
    sudo chown www-data:adm /var/log/apache2
}

# Restart Apache and update AWStats
restart_services() {
    echo "Restarting Apache..."
    if command -v systemctl >/dev/null 2>&1; then
        sudo systemctl restart apache2
    else
        sudo /etc/init.d/apache2 restart
    fi

    echo "Updating AWStats statistics..."
    sudo -u www-data /usr/lib/cgi-bin/awstats.pl -config=awstats -update
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_commands sudo apt-get chmod chown vi systemctl apache2
    check_sudo

    install_awstats
    configure_awstats
    restart_services

    echo "AWStats installation and configuration completed successfully."
}

# Execute main function
main "$@"

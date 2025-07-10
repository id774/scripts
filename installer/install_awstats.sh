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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script to install and configure AWStats:
#      ./install_awstats.sh
#
#  Requirements:
#  - Must be executed with sudo privileges.
#  - Apache2 must be installed on the system.
#
#  Version History:
#  v1.4 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.3 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-15
#       Added system, command, and sudo checks.
#       Improved error handling and permission settings.
#       Ensured idempotent execution.
#  v0.1 2011-09-07
#       Initial version.
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Install AWStats package
install_awstats() {
    echo "[INFO] Installing AWStats."
    if ! sudo apt-get update; then
        echo "[ERROR] Failed to update package list." >&2
        exit 1
    fi

    if ! sudo apt-get install -y awstats; then
        echo "[ERROR] Failed to install AWStats." >&2
        exit 1
    fi
}

# Configure AWStats
configure_awstats() {
    echo "[INFO] Configuring AWStats and Apache."

    for file in /etc/awstats/awstats.conf* /etc/apache2/sites-available/custom* /etc/logrotate.d/apache2; do
        if [ ! -f "$file" ]; then
            echo "[WARN] File $file does not exist. Skipping edit." >&2
        else
            echo "[INFO] Please edit $file."
        fi
    done

    echo "[INFO] Setting permissions on Apache logs."
    if ! sudo chmod 440 /var/log/apache2/*; then
        echo "[ERROR] Failed to set log file permissions." >&2
        exit 1
    fi

    if ! sudo chown www-data:adm /var/log/apache2/*; then
        echo "[ERROR] Failed to change ownership of log files." >&2
        exit 1
    fi

    if ! sudo chmod 550 /var/log/apache2; then
        echo "[ERROR] Failed to set directory permissions." >&2
        exit 1
    fi

    if ! sudo chown www-data:adm /var/log/apache2; then
        echo "[ERROR] Failed to change ownership of log directory." >&2
        exit 1
    fi
}

# Restart Apache and update AWStats
restart_services() {
    echo "[INFO] Restarting Apache..."
    if command -v systemctl >/dev/null 2>&1; then
        if ! sudo systemctl restart apache2; then
            echo "[ERROR] Failed to restart Apache with systemctl." >&2
            exit 1
        fi
    else
        if ! sudo /etc/init.d/apache2 restart; then
            echo "[ERROR] Failed to restart Apache with init.d." >&2
            exit 1
        fi
    fi

    echo "[INFO] Updating AWStats statistics..."
    if ! sudo -u www-data /usr/lib/cgi-bin/awstats.pl -config=awstats -update; then
        echo "[ERROR] Failed to update AWStats statistics." >&2
        exit 1
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands sudo apt-get chmod chown systemctl apache2
    check_sudo

    install_awstats
    configure_awstats
    restart_services

    echo "[INFO] AWStats installation and configuration completed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

#!/bin/sh

########################################################################
# setup_apache2_ssl.sh: Configure Apache2 with SSL
#
#  Description:
#  This script automates the setup of an SSL-enabled Apache2 server by:
#  - Generating an SSL certificate for Apache2.
#  - Deploying custom site configuration files.
#  - Enabling SSL and configuring virtual hosts.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script to configure Apache2 with SSL:
#      ./setup_apache2_ssl.sh
#
#  Requirements:
#  - Must be executed with sudo privileges.
#  - The `$SCRIPTS` environment variable must be set.
#  - Apache2 must be installed on the system.
#
#  Version History:
#  v1.4 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.3 2025-04-26
#       Exit immediately on critical failures and
#       ensure success message is shown only if all steps succeed.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-15
#       Added command, script, and sudo checks.
#       Improved error handling and permission settings.
#       Ensured idempotent execution.
#  v0.1 2011-04-14
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

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the Apache2 with SSL configuration files." >&2
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

# Function to deploy SSL certificate
deploy_ssl_cert() {
    echo "[INFO] Setting up SSL certificate..."
    if [ ! -d /etc/apache2/ssl ]; then
        sudo mkdir -p /etc/apache2/ssl
    fi
    if ! sudo make-ssl-cert /usr/share/ssl-cert/ssleay.cnf /etc/apache2/ssl/apache.pem; then
        echo "[ERROR] Failed to generate SSL certificate." >&2
        exit 1
    fi
}

# Function to deploy Apache site configuration files
deploy_site_configs() {
    echo "[INFO] Deploying site configurations..."
    for site in custom custom-ssl; do
        if ! sudo cp "$SCRIPTS/etc/apache/$site" /etc/apache2/sites-available/; then
            echo "[ERROR] Failed to copy $site configuration." >&2
            exit 1
        fi
        sudo chmod 644 /etc/apache2/sites-available/$site
        sudo chown root:root /etc/apache2/sites-available/$site
        echo "[INFO] Please edit /etc/apache2/sites-available/$site"
    done
}

# Function to configure Apache2
configure_apache() {
    echo "[INFO] Configuring Apache2..."
    if ! sudo a2enmod ssl; then
        echo "[ERROR] Failed to enable SSL module." >&2
        exit 1
    fi
    if ! sudo a2dissite default; then
        echo "[ERROR] Failed to disable default site." >&2
        exit 1
    fi
    if ! sudo a2dissite default-ssl; then
        echo "[ERROR] Failed to disable default-ssl site." >&2
        exit 1
    fi
    if ! sudo a2ensite custom; then
        echo "[ERROR] Failed to enable custom site." >&2
        exit 1
    fi
    if ! sudo a2ensite custom-ssl; then
        echo "[ERROR] Failed to enable custom-ssl site." >&2
        exit 1
    fi
    if ! sudo systemctl reload apache2; then
        if ! sudo /etc/init.d/apache2 reload; then
            echo "[ERROR] Failed to reload Apache2 service." >&2
            exit 1
        fi
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands sudo systemctl apache2 make-ssl-cert a2enmod a2ensite a2dissite
    check_scripts
    check_sudo

    deploy_ssl_cert
    deploy_site_configs
    configure_apache

    echo "[INFO] Apache2 SSL setup completed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

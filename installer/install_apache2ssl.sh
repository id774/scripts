#!/bin/sh

########################################################################
# install_apache2ssl.sh: Configure Apache2 with SSL
#
#  Description:
#  This script automates the setup of an SSL-enabled Apache2 server by:
#  - Generating an SSL certificate for Apache2.
#  - Deploying custom site configuration files.
#  - Enabling SSL and configuring virtual hosts.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-15
#       Added command, script, and sudo checks.
#       Improved error handling and permission settings.
#       Ensured idempotent execution.
#  v0.1 2011-04-14
#       Initial version.
#
#  Usage:
#  Run this script to configure Apache2 with SSL:
#      ./install_apache2ssl.sh
#
#  Requirements:
#  - Must be executed with sudo privileges.
#  - The `$SCRIPTS` environment variable must be set.
#  - Apache2 must be installed on the system.
#
########################################################################

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

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the Apache2 with SSL configuration files." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Function to deploy SSL certificate
deploy_ssl_cert() {
    echo "Setting up SSL certificate..."
    if [ ! -d /etc/apache2/ssl ]; then
        sudo mkdir -p /etc/apache2/ssl
    fi
    sudo make-ssl-cert /usr/share/ssl-cert/ssleay.cnf /etc/apache2/ssl/apache.pem
}

# Function to deploy Apache site configuration files
deploy_site_configs() {
    echo "Deploying site configurations..."
    for site in custom custom-ssl; do
        sudo cp "$SCRIPTS/etc/apache/$site" /etc/apache2/sites-available/
        sudo chmod 644 /etc/apache2/sites-available/$site
        sudo chown root:root /etc/apache2/sites-available/$site
        echo "Please edit /etc/apache2/sites-available/$site"
    done
}

# Function to configure Apache2
configure_apache() {
    echo "Configuring Apache2..."
    sudo a2enmod ssl
    sudo a2dissite default
    sudo a2dissite default-ssl
    sudo a2ensite custom
    sudo a2ensite custom-ssl
    sudo systemctl reload apache2 || sudo /etc/init.d/apache2 reload
}

# Main function to execute the script
main() {
    check_system
    check_commands sudo systemctl apache2 make-ssl-cert a2enmod a2ensite a2dissite
    check_scripts
    check_sudo

    deploy_ssl_cert
    deploy_site_configs
    configure_apache

    echo "Apache2 SSL setup completed successfully."
}

# Execute main function
main "$@"

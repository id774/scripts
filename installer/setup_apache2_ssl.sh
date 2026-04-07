#!/bin/sh

########################################################################
# setup_apache2_ssl.sh: Configure Apache2 with SSL
#
#  Description:
#  This script automates the setup of an SSL-enabled Apache2 server by:
#  - Generating an SSL certificate for Apache2.
#  - Deploying Apache site configuration templates.
#  - Installing deployed files under host-specific FQDN-based names.
#  - Enabling SSL and configuring virtual hosts.
#
#  Template deployment:
#  - The configuration files distributed with this script are templates.
#  - They are stored under fixed template names and do not need to be
#    renamed before execution.
#  - This script detects the current host FQDN and copies each template
#    to /etc/apache2/sites-available/ using host-specific file names.
#  - The deployed files are intended to be reviewed and edited on each
#    target host after deployment.
#
#  Naming convention:
#  - Distributed template files use the following fixed names:
#      hostname.sitename.conf
#      hostname.sitename-ssl.conf
#  - Example template names:
#      hostname.sitename.conf
#      hostname.sitename-ssl.conf
#  - On deployment, these templates are installed using the detected
#    host FQDN:
#      <host-fqdn>.conf
#      <host-fqdn>-ssl.conf
#  - Example installed file names on host wyvern.id774.net:
#      wyvern.id774.net.conf
#      wyvern.id774.net-ssl.conf
#  - This keeps the distributed template names stable while ensuring
#    that the deployed file names match the target host identity.
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
#  - Template files must exist under:
#      $SCRIPTS/etc/apache/sites-available/hostname.sitename.conf
#      $SCRIPTS/etc/apache/sites-available/hostname.sitename-ssl.conf
#
#  Version History:
#  v1.6 2026-04-07
#       Adopt FQDN-based naming and deploy Apache configs from templates with host-specific installation.
#  v1.5 2025-08-27
#       Rename Apache custom configs to use .conf suffix.
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

# Check if the system is Linux
check_system() {
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
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
        echo "[ERROR] Please set SCRIPTS to the directory containing Apache templates." >&2
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

# Detect target host FQDN
detect_host_fqdn() {
    HOST_FQDN=$(hostname -f 2>/dev/null)
    if [ -z "$HOST_FQDN" ]; then
        echo "[ERROR] Failed to detect the host FQDN using hostname -f." >&2
        exit 1
    fi
}

# Check template files
check_templates() {
    for template in hostname.sitename.conf hostname.sitename-ssl.conf; do
        if [ ! -f "$SCRIPTS/etc/apache/sites-available/$template" ]; then
            echo "[ERROR] Missing template file: $SCRIPTS/etc/apache/sites-available/$template" >&2
            exit 1
        fi
    done
}

# Deploy SSL certificate
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

# Deploy Apache site configuration files from fixed templates
deploy_site_configs() {
    echo "[INFO] Deploying site configurations..."

    if ! sudo cp "$SCRIPTS/etc/apache/sites-available/hostname.sitename.conf" \
        "/etc/apache2/sites-available/$HOST_FQDN.conf"; then
        echo "[ERROR] Failed to copy hostname.sitename.conf." >&2
        exit 1
    fi

    if ! sudo cp "$SCRIPTS/etc/apache/sites-available/hostname.sitename-ssl.conf" \
        "/etc/apache2/sites-available/$HOST_FQDN-ssl.conf"; then
        echo "[ERROR] Failed to copy hostname.sitename-ssl.conf." >&2
        exit 1
    fi

    sudo chmod 0644 "/etc/apache2/sites-available/$HOST_FQDN.conf"
    sudo chmod 0644 "/etc/apache2/sites-available/$HOST_FQDN-ssl.conf"
    sudo chown root:root "/etc/apache2/sites-available/$HOST_FQDN.conf"
    sudo chown root:root "/etc/apache2/sites-available/$HOST_FQDN-ssl.conf"

    echo "[INFO] Installed /etc/apache2/sites-available/$HOST_FQDN.conf"
    echo "[INFO] Installed /etc/apache2/sites-available/$HOST_FQDN-ssl.conf"
    echo "[INFO] Please review and edit the deployed files as needed."
}

# Configure Apache2
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
    if ! sudo a2ensite "$HOST_FQDN"; then
        echo "[ERROR] Failed to enable $HOST_FQDN site." >&2
        exit 1
    fi
    if ! sudo a2ensite "$HOST_FQDN-ssl"; then
        echo "[ERROR] Failed to enable $HOST_FQDN-ssl site." >&2
        exit 1
    fi
    if ! sudo systemctl reload apache2; then
        if ! sudo /etc/init.d/apache2 reload; then
            echo "[ERROR] Failed to reload Apache2 service." >&2
            exit 1
        fi
    fi
}

# Main entry point
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands sudo systemctl apache2 make-ssl-cert a2enmod a2ensite a2dissite hostname
    check_scripts
    check_sudo
    detect_host_fqdn
    check_templates

    deploy_ssl_cert
    deploy_site_configs
    configure_apache

    echo "[INFO] Apache2 SSL setup completed successfully."
    return 0
}

# Execute main function
main "$@"

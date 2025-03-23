#!/bin/sh

########################################################################
# install_R_libs.sh: Installer for R Libraries
#
#  Description:
#  This script installs a predefined set of R libraries by sourcing
#  the configuration file `install_mylibs.R`.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-16
#       Stable version, installs R libraries from config file.
#  v0.1 2014-08-09
#       Initial version.
#
#  Usage:
#  Run this script to install all required R libraries:
#      ./install_R_libs.sh
#
#  Requirements:
#  - The user must have `R` installed.
#  - The script must be run with `sudo` if installing system-wide.
#  - This script is intended for Linux systems only.
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

# Function to check if R is installed
check_R() {
    if ! command -v R >/dev/null 2>&1; then
        echo "Error: R is not installed. Please install R and try again." >&2
        exit 1
    fi
}

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the config file." >&2
        exit 1
    fi
}

# Function to check if the config file exists
check_config() {
    if [ ! -f "$SCRIPTS/etc/install_mylibs.R" ]; then
        echo "Error: Configuration file install_mylibs.R not found in $SCRIPTS/etc/." >&2
        exit 1
    fi
}

# Function to check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Function to install R libraries
install_R_libs() {
    echo "Installing R libraries..."
    sudo R --no-save --no-restore -e "source('$SCRIPTS/etc/install_mylibs.R'); install.packages(required_packages, dependencies=TRUE)"
    echo "R library installation completed."
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_R
    check_scripts
    check_config
    check_sudo
    install_R_libs
}

# Execute main function
main "$@"

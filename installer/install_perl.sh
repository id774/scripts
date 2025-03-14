#!/bin/sh

########################################################################
# install_perl.sh: Installer for Perl
#
#  Description:
#  This script automates the installation of Perl by:
#  - Downloading the specified or default version from CPAN.
#  - Compiling and installing the package.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.5 2025-03-14
#       Added network connection check, Linux system validation, command validation, and improved argument handling.
#  v0.4 2010-09-16
#       Refactoring.
#  v0.3 2010-03-07
#       Refactoring.
#  v0.2 2010-02-20
#       Refactoring.
#  v0.1 2009-06-01
#       First version, only for 5.10.0.
#
#  Usage:
#  Run this script without arguments to install the default version (5.10.0):
#      ./install_perl.sh /usr/local/perl
#  Specify a version to install a different release:
#      ./install_perl.sh 5.32.1 /opt/perl
#  Skip saving sources by adding a third argument:
#      ./install_perl.sh 5.32.1 /opt/perl -n
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, `sudo`, and `tar` installed.
#  - Must be executed in a shell environment with internet access.
#  - This script is intended for Linux systems only.
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
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to check network connectivity
check_network() {
    if ! ping -c 1 id774.net >/dev/null 2>&1; then
        echo "Error: No network connection detected. Please check your internet access." >&2
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

# Setup version and environment
setup_environment() {
    VERSION="${1:-5.10.0}"
    PREFIX="$2"
    if [ "$3" = "no-sudo" ]; then
        SUDO=""
    else
        SUDO="sudo"
    fi
    case "$OSTYPE" in
        *darwin*) OPTIONS="-pR"; OWNER="root:wheel" ;;
        *) OPTIONS="-a"; OWNER="root:root" ;;
    esac
}

# Save sources if requested
save_sources() {
    sudo mkdir -p /usr/local/src/perl
    sudo cp $OPTIONS "perl-$VERSION" /usr/local/src/perl
    sudo chown -R $OWNER /usr/local/src/perl
}

# Build and install Perl
make_and_install() {
    cd "perl-$VERSION" || exit 1
    if [ -z "$PREFIX" ]; then
        ./Configure
    else
        ./Configure -des -Dprefix="$PREFIX"
    fi
    make
    $SUDO make install
    cd ..
}

# Download and extract Perl
get_perl() {
    mkdir install_perl
    cd install_perl || exit 1
    wget "http://www.cpan.org/authors/id/R/RG/RGARCIA/perl-$VERSION.tar.gz"
    if [ ! -f "perl-$VERSION.tar.gz" ]; then
        echo "Error: Failed to download Perl $VERSION." >&2
        exit 1
    fi
    tar xzvf "perl-$VERSION.tar.gz"
    if [ "$2" != "sourceonly" ]; then
        make_and_install
    fi
    [ -n "$3" ] || save_sources
    cd ..
    rm -rf install_perl
}

# Install Perl
install_perl() {
    setup_environment "$1" "$2" "$3"
    if [ -z "$2" ]; then
        echo "Error: Installation prefix must be specified." >&2
        exit 1
    fi
    get_perl "$1" "$2" "$3"
    perl -V
}

# Main execution function
main() {
    # Perform initial checks
    check_system
    check_commands wget make sudo tar ping
    check_network
    check_sudo

    # Run the installation process
    install_perl "$1" "$2" "$3"

    echo "Perl $VERSION installed successfully."
}

main "$@"

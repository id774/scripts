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
#  v1.0 2025-03-19
#       Added network connection check, Linux system validation, command validation, and improved argument handling.
#  [Further version history truncated for brevity]
#  v0.1 2009-06-01
#       First version, only for 5.10.0.
#
#  Usage:
#  Run this script without arguments to install the default version (5.10.0):
#      ./install_perl.sh
#  Specify a different Python version:
#      ./install_perl.sh 5.32.1
#  Specify an installation prefix:
#      ./install_perl.sh 5.10.1 /opt/perl/5.10
#  Run without sudo (for local installation):
#      ./install_perl.sh 5.32.1 ~/.local/perl --no-sudo
#  Skip saving sources by adding a fourth argument:
#      ./install_perl.sh 5.32.1 /opt/perl sudo -n
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, and `tar` installed.
#  - Must be executed in a shell environment with internet access.
#
########################################################################

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

# Function to check network connectivity
check_network() {
    if ! curl -s --head --connect-timeout 5 http://clients3.google.com/generate_204 >/dev/null; then
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
    MAJOR_MINOR="$(echo "$VERSION" | awk -F. '{print $1"."$2}')"
    PREFIX="${2:-/opt/perl/$MAJOR_MINOR}"

    if [ -z "$3" ] || [ "$3" = "sudo" ]; then
        SUDO="sudo"
    else
        SUDO=""
    fi
    [ "$SUDO" = "sudo" ] && check_sudo

    case "$OSTYPE" in
        *darwin*) OPTIONS="-pR"; OWNER="root:wheel" ;;
        *) OPTIONS="-a"; OWNER="root:root" ;;
    esac
}

# Save sources if requested
save_sources() {
    [ "$SUDO" = "sudo" ] || return
    $SUDO mkdir -p /usr/local/src/perl
    $SUDO cp $OPTIONS "perl-$VERSION" /usr/local/src/perl
    $SUDO chown $OWNER /usr/local/src/perl
    $SUDO chown -R $OWNER /usr/local/src/perl/perl-$VERSION
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
    cd .. || exit 1
}

# Download and extract Perl
get_perl() {
    mkdir install_perl
    cd install_perl || exit 1
    wget "http://www.cpan.org/authors/id/R/RG/RGARCIA/perl-$VERSION.tar.gz"

    # Check if the file was downloaded successfully
    if [ ! -f "perl-$VERSION.tar.gz" ]; then
        echo "Error: Failed to download Perl $VERSION." >&2
        exit 1
    fi

    tar xzvf "perl-$VERSION.tar.gz"
    make_and_install
    [ -n "$4" ] || save_sources
    cd .. || exit 1
    $SUDO rm -rf install_perl
}

# Install Perl
install_perl() {
    if [ -z "$2" ]; then
        echo "Error: Installation prefix must be specified." >&2
        exit 1
    fi
    get_perl "$@"
    perl -V
}

# Main function to execute the script
main() {
    # Perform initial checks
    check_commands curl wget make sudo tar awk mkdir cp chown
    check_network

    # Run the installation process
    setup_environment "$@"
    install_perl "$VERSION" "$PREFIX" "$SUDO" "$4"

    echo "Perl $VERSION installed successfully."
}

# Execute main function
main "$@"

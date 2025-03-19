#!/bin/sh

########################################################################
# install_python.sh: Installer for Python
#
#  Description:
#  This script automates the installation of Python by:
#  - Downloading the specified or default version from the official site.
#  - Compiling and installing the package.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.0 2025-03-19
#       Added network connection check, system validation, command validation, and improved argument handling.
#       Default Python version 3.13.2 installs in '/opt/python/3.13' directory.
#       Improved directory navigation safety.
#       Set default installation path to /opt/python/x.x.
#  [Further version history truncated for brevity]
#  v1.0 2009-01-07
#       First stable release.
#
#  Usage:
#  Run this script without arguments to install the default Python version (3.13.2):
#      ./install_python.sh
#  Specify a different Python version:
#      ./install_python.sh 3.12.9
#  Specify an installation prefix:
#      ./install_python.sh 3.12.9 /opt/python/3.12
#  Run without sudo (for local installation):
#      ./install_python.sh 3.13.2 ~/.local/python --no-sudo
#  Skip saving sources by adding a fourth argument:
#      ./install_python.sh 3.13.2 /opt/python sudo -n
#
#  By default, if no installation path is provided, the script will install Python under /opt/python/x.x.
#  For example, Python 3.13.2 will be installed in /opt/python/3.13.
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `curl`, `make`, and `tar` installed.
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
    if [ "$SUDO" = "sudo" ] && ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access or specify 'no-sudo'." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    VERSION="${1:-3.13.2}"
    MAJOR_MINOR="$(echo "$VERSION" | awk -F. '{print $1"."$2}')"
    PREFIX="${2:-/opt/python/$MAJOR_MINOR}"

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
    $SUDO mkdir -p /usr/local/src/python
    $SUDO cp $OPTIONS "Python-$VERSION" /usr/local/src/python
    $SUDO chown $OWNER /usr/local/src/python
    $SUDO chown -R $OWNER /usr/local/src/python/Python-$VERSION
}

# Compile and install Python
make_and_install() {
    cd "Python-$VERSION" || exit 1
    ./configure --prefix="$PREFIX"
    make
    $SUDO make install
    cd .. || exit 1
}

# Download and install Python
install_python() {
    mkdir install_python
    cd install_python || exit 1
    curl -L "http://www.python.org/ftp/python/$VERSION/Python-$VERSION.tgz" -O

    # Check if the file was downloaded successfully
    if [ ! -f "Python-$VERSION.tgz" ]; then
        echo "Error: Failed to download Python $VERSION." >&2
        exit 1
    fi

    tar xzvf "Python-$VERSION.tgz"
    make_and_install
    [ -n "$4" ] || save_sources
    cd .. || exit 1
    $SUDO rm -rf install_python
}

# Main function to execute the script
main() {
    # Perform initial checks
    check_commands curl sudo make tar awk mkdir cp chown
    check_network

    # Run the installation process
    setup_environment "$@"
    install_python "$VERSION" "$PREFIX" "$SUDO" "$4"

    echo "Python $VERSION installed successfully in $PREFIX."
}

# Execute main function
main "$@"

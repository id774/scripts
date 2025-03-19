#!/bin/sh

########################################################################
# install_python.sh: Installer for Python
#
#  Description:
#  This script automates the installation of Python by:
#  - Downloading the specified or default version from the official site.
#  - Compiling and installing the package.
#  - Optionally saving the source files for future use.
#  - Creating symlinks for Python, pip, and ipython.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.0 2025-03-19
#       Added network connection check, system validation, command validation, and improved argument handling.
#       Set default installation path to /opt/python/x.x.
#  v1.9 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.8 2014-06-26
#       Change address of pip.
#  v1.7 2014-03-12
#       Create symlink for ipython3.
#  v1.6 2014-02-14
#       Change source tarball ext.
#  v1.5 2014-02-09
#       Easy install as default, create symlink, bug fix.
#  v1.4 2010-09-16
#       Refactoring.
#  v1.3 2010-03-07
#       Refactoring.
#  v1.2 2010-02-20
#       Refactoring.
#  v1.1 2009-02-21
#       Add sourceonly option.
#  v1.0 2009-01-07
#       Stable.
#
#  Usage:
#  Run this script without arguments to install the default version:
#      ./install_python.sh 3.13.2
#  Specify an installation prefix:
#      ./install_python.sh 3.13.2 /opt/python/3.13
#  Run without sudo (for local installation):
#      ./install_python.sh 3.13.2 ~/.local/python no-sudo
#  Skip saving sources by adding a third argument:
#      ./install_python.sh 3.13.2 /opt/python sudo -n
#
#  By default, if no installation path is provided, the script will install Python under /opt/python/x.x.
#  For example, Python 3.13.2 will be installed in /opt/python/3.13.
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `curl`, `make`, `sudo`, and `tar` installed.
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

    if [ "$3" = "no-sudo" ]; then
        SUDO=""
    else
        SUDO="sudo"
    fi
    check_sudo

    case $OSTYPE in
      *darwin*)
        OPTIONS=-pR
        OWNER=root:wheel
        ;;
      *)
        OPTIONS=-a
        OWNER=root:root
        ;;
    esac
}

# Save sources if requested
save_sources() {
    $SUDO mkdir -p /usr/local/src/python
    $SUDO cp $OPTIONS "Python-$1" /usr/local/src/python
    $SUDO chown $OWNER /usr/local/src/python
    $SUDO chown -R $OWNER /usr/local/src/python/Python-$1
}

# Compile and install Python
make_and_install() {
    cd "Python-$1" || exit 1
    ./configure --prefix="$PREFIX"
    make
    $SUDO make install
    cd ..
}

# Download and extract Python
install_python() {
    mkdir install_python
    cd install_python || exit 1
    curl -L "http://www.python.org/ftp/python/$1/Python-$1.tgz" -O
    if [ ! -f "Python-$1.tgz" ]; then
        echo "Error: Failed to download Python $1." >&2
        exit 1
    fi
    tar xzvf "Python-$1.tgz"
    [ "$2" = "sourceonly" ] || make_and_install "$1" "$2"
    [ -n "$4" ] || save_sources "$1"
    cd ..
    $SUDO rm -rf install_python
}

# Main function to execute the script
main() {
    # Perform initial checks
    check_commands curl make sudo tar awk
    check_network

    # Run the installation process
    setup_environment "$1" "$2" "$3" "$4"
    install_python "$1" "$2" "$3" "$4"

    echo "Python $1 installed successfully in $PREFIX."
}

# Execute main function
main "$@"

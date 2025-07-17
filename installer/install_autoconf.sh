#!/bin/sh

########################################################################
# install_autoconf.sh: Installer for Autoconf
#
#  Description:
#  This script automates the installation of Autoconf by:
#  - Downloading the specified or default version from the GNU FTP server.
#  - Compiling and installing it to the system.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script without arguments to install the default version (2.72):
#      ./install_autoconf.sh
#
#  Specify a version to install a different release:
#      ./install_autoconf.sh 2.71
#
#  Skip saving sources by specifying any second argument:
#      ./install_autoconf.sh 2.71 skip
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, and `sudo` installed.
#  - Must be executed in a shell environment with internet access.
#  - This script is intended for Linux systems only.
#
#  Version History:
#  v0.8 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v0.7 2025-04-27
#       Add error handling for critical installation steps to ensure robust build and installation.
#  v0.6 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v0.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v0.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v0.3 2025-03-20
#       Update default Autoconf version to 2.72 and adjust installation prefix.
#  v0.2 2025-03-14
#       Added network connection check and improved argument handling.
#  v0.1 2011-04-26
#       First version.
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
    if [ "$(uname -s)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check required commands
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

# Check network connectivity
check_network() {
    if ! curl -s --head --connect-timeout 5 http://clients3.google.com/generate_204 >/dev/null; then
        echo "[ERROR] No network connection detected. Please check your internet access." >&2
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

# Setup version environment
setup_environment() {
    VERSION="${1:-2.72}"
}

# Save sources if requested
save_sources() {
    echo "[INFO] Saving sources to /usr/local/src/autoconf."
    if ! sudo mkdir -p /usr/local/src/autoconf; then
        echo "[ERROR] Failed to create /usr/local/src/autoconf." >&2
        exit 1
    fi
    if ! sudo cp -av "autoconf-$VERSION" /usr/local/src/autoconf/; then
        echo "[ERROR] Failed to copy autoconf-$VERSION to /usr/local/src/autoconf." >&2
        exit 1
    fi
    sudo chown -R root:root /usr/local/src/autoconf
}

# Install Autoconf
install_autoconf() {
    setup_environment "$1"

    echo "[INFO] Creating temporary build directory."
    mkdir install_autoconf
    cd install_autoconf || exit 1

    echo "[INFO] Downloading Autoconf $VERSION."
    if ! wget "ftp://ftp.gnu.org/gnu/autoconf/autoconf-$VERSION.tar.gz"; then
        echo "[ERROR] Failed to download autoconf-$VERSION.tar.gz." >&2
        exit 1
    fi

    echo "[INFO] Extracting archive."
    if ! tar xzvf "autoconf-$VERSION.tar.gz"; then
        echo "[ERROR] Failed to extract autoconf-$VERSION.tar.gz." >&2
        exit 1
    fi

    echo "[INFO] Configuring build."
    cd "autoconf-$VERSION" || exit 1
    if ! ./configure --prefix=/usr/local; then
        echo "[ERROR] Failed to configure autoconf-$VERSION." >&2
        exit 1
    fi

    echo "[INFO] Building Autoconf."
    if ! make; then
        echo "[ERROR] Failed to build autoconf-$VERSION." >&2
        exit 1
    fi

    echo "[INFO] Installing Autoconf."
    if ! sudo make install; then
        echo "[ERROR] Failed to install autoconf-$VERSION." >&2
        exit 1
    fi

    cd .. || exit 1
    [ -n "$2" ] || save_sources
    cd .. || exit 1

    echo "[INFO] Cleaning up temporary files."
    rm -rf install_autoconf
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Perform initial checks
    check_system
    check_commands curl wget make sudo tar mkdir cp rm chown
    check_network
    check_sudo

    # Run the installation process
    install_autoconf "$1" "$2"

    echo "[INFO] Autoconf $VERSION installed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

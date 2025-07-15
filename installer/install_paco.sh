#!/bin/sh

########################################################################
# install_paco.sh: Installer for paco
#
#  Description:
#  This script automates the installation of paco by:
#  - Downloading the specified or default version from SourceForge.
#  - Compiling and installing the package.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script without arguments to install the default version (2.0.9):
#      ./install_paco.sh
#
#  Specify a version to install a different release:
#      ./install_paco.sh 2.0.8
#
#  Skip saving sources by specifying any second argument:
#      ./install_paco.sh 2.0.9 skip
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, `sudo`, and `tar` installed.
#  - Must be executed in a shell environment with internet access.
#  - This script is intended for Linux systems only.
#
#  Version History:
#  v1.9 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.8 2025-04-27
#       Add strict error checking for make and make install processes during paco installation.
#  v1.7 2025-04-22
#       Improved log granularity with [INFO] and [ERROR] tags for each step.
#  v1.6 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.5 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.4 2025-03-14
#       Added network connection check, Linux system validation, command validation, and improved argument handling.
#  v1.3 2010-09-16
#       Refactoring.
#  v1.2 2010-03-07
#       Refactoring and update to 2.0.7.
#  v1.1 2008-12-15
#       Keep sources.
#  v1.0 2008-12-04
#       Stable.
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

# Function to check network connectivity
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

# Setup version and environment
setup_environment() {
    PACO_VERSION="${1:-2.0.9}"
}

# Save sources if requested
save_sources() {
    echo "[INFO] Saving sources to /usr/local/src/paco."
    sudo mkdir -p /usr/local/src/paco
    if ! sudo cp -av "paco-$PACO_VERSION" /usr/local/src/paco/; then
        echo "[ERROR] Failed to copy source directory to /usr/local/src/paco" >&2
        exit 1
    fi
    sudo chown -R root:root /usr/local/src/paco
}

# Install paco
install_paco() {
    setup_environment "$1"

    echo "[INFO] Creating temporary build directory: install_paco"
    mkdir install_paco
    cd install_paco || exit 1

    echo "[INFO] Downloading paco version $PACO_VERSION..."
    if ! wget "http://downloads.sourceforge.net/paco/paco-$PACO_VERSION.tar.gz"; then
        echo "[ERROR] Failed to download paco-$PACO_VERSION.tar.gz" >&2
        exit 1
    fi
    echo "[INFO] Download complete: paco-$PACO_VERSION.tar.gz."

    echo "[INFO] Extracting archive..."
    if ! tar xzvf "paco-$PACO_VERSION.tar.gz"; then
        echo "[ERROR] Failed to extract paco-$PACO_VERSION.tar.gz." >&2
        exit 1
    fi

    echo "[INFO] Configuring paco..."
    cd "paco-$PACO_VERSION" || exit 1
    ./configure --disable-gpaco

    echo "[INFO] Building paco..."
    if ! make; then
        echo "[ERROR] Build failed during make." >&2
        exit 1
    fi

    echo "[INFO] Installing paco..."
    if ! sudo make install; then
        echo "[ERROR] Install failed during make install." >&2
        exit 1
    fi

    echo "[INFO] Logging installation..."
    if ! sudo make logme; then
        echo "[ERROR] Failed to log installation." >&2
        exit 1
    fi

    cd .. || exit 1
    [ -n "$2" ] || save_sources
    cd .. || exit 1

    echo "[INFO] Cleaning up build directory..."
    rm -rf install_paco
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Perform initial checks
    check_system
    check_commands curl wget make sudo tar mkdir chown cp rm
    check_network
    check_sudo

    # Run the installation process
    install_paco "$1" "$2"

    echo "[INFO] paco $PACO_VERSION installed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

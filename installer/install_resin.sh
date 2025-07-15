#!/bin/sh

########################################################################
# install_resin.sh: Installer for Resin
#
#  Description:
#  This script automates the installation of Resin by:
#  - Downloading the specified or default version from the official site.
#  - Compiling and installing the application.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script without arguments to install the default version (4.0.67):
#      ./install_resin.sh
#
#  Specify a version to install a different release:
#      ./install_resin.sh 4.0.66
#
#  Skip saving sources by specifying any second argument:
#      ./install_resin.sh 4.0.66 skip
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, `sudo`, `unzip`, and `tar` installed.
#  - Must be executed in a shell environment with internet access.
#  - This script is intended for Linux systems only.
#
#  Version History:
#  v0.8 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v0.7 2025-04-27
#       Add error handling for critical build and install steps
#  v0.6 2025-04-22
#       Improved log granularity with [INFO] and [ERROR] tags for each step.
#  v0.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v0.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v0.3 2025-03-14
#       Added network connection check, Linux system validation, command validation, and improved argument handling.
#  v0.2 2011-06-22
#       Version added to installation target.
#  v0.1 2011-06-16
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
    VERSION="${1:-4.0.67}"
    RESIN="resin-$VERSION"
}

# Save sources if requested
save_sources() {
    echo "[INFO] Saving sources to /usr/local/src/resin"
    sudo mkdir -p /usr/local/src/resin

    if ! sudo cp -av "$RESIN" /usr/local/src/resin/; then
        echo "[ERROR] Failed to copy source directory $RESIN" >&2
        exit 1
    fi

    sudo chown -R root:root /usr/local/src/resin
}

# Install Resin
install_resin() {
    setup_environment "$1"

    echo "[INFO] Creating temporary build directory: install_resin."
    mkdir install_resin
    cd install_resin || exit 1

    echo "[INFO] Downloading Resin $VERSION..."
    if ! wget "http://www.caucho.com/download/$RESIN.zip"; then
        echo "[ERROR] Failed to download Resin $VERSION." >&2
        exit 1
    fi
    echo "[INFO] Download complete: $RESIN.zip"

    echo "[INFO] Extracting archive..."
    if ! unzip "$RESIN.zip"; then
        echo "[ERROR] Failed to extract $RESIN.zip." >&2
        exit 1
    fi

    echo "[INFO] Configuring Resin..."
    cd "$RESIN" || exit 1
    if ! ./configure --prefix="/opt/resin/$VERSION"; then
        echo "[ERROR] Configure step failed." >&2
        exit 1
    fi

    echo "[INFO] Building Resin..."
    if ! make; then
        echo "[ERROR] Build step failed." >&2
        exit 1
    fi

    echo "[INFO] Installing Resin..."
    if ! sudo make install; then
        echo "[ERROR] Install step failed." >&2
        exit 1
    fi

    cd .. || exit 1
    [ -n "$2" ] || save_sources
    cd .. || exit 1

    echo "[INFO] Cleaning up temporary files..."
    rm -rf install_resin
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Perform initial checks
    check_system
    check_commands curl wget make sudo unzip tar chown mkdir cp rm
    check_network
    check_sudo

    # Run the installation process
    install_resin "$1" "$2"

    echo "[INFO] Resin $VERSION installed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

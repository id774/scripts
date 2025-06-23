#!/bin/sh

########################################################################
# install_talib.sh: Installer for TA-Lib
#
#  Description:
#  This script automates the installation of TA-Lib by:
#  - Downloading the specified or default version from the archive site.
#  - Compiling and installing the package.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.8 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v0.7 2025-04-27
#       Add critical failure checks to TA-Lib installation process.
#  v0.6 2025-04-21
#       Refined [INFO] log messages for clearer and more meaningful execution steps.
#  v0.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v0.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v0.3 2025-03-14
#       Added sudo privilege check, system validation, command validation, and improved argument handling.
#  v0.2 2015-05-31
#       Change URL.
#  v0.1 2015-03-23
#       First version.
#
#  Usage:
#  Run this script without arguments to install the default version (0.4.0):
#      ./install_talib.sh
#  Specify a version to install a different release:
#      ./install_talib.sh 0.4.1
#  Skip saving sources by adding a second argument:
#      ./install_talib.sh 0.4.1 -n
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `curl`, `make`, `sudo`, and `tar` installed.
#  - Must be executed in a shell environment with internet access.
#  - This script is intended for Linux systems only.
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

# Function to check if user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    VERSION="${1:-0.4.0}"
    FILENAME="ta-lib-$VERSION-src.tar.gz"
}

# Save sources if requested
save_sources() {
    echo "[INFO] Saving sources to /usr/local/src/ta-lib"

    if ! sudo mkdir -p /usr/local/src; then
        echo "[ERROR] Failed to create /usr/local/src." >&2
        exit 1
    fi

    if ! sudo cp -av ta-lib /usr/local/src/; then
        echo "[ERROR] Failed to copy ta-lib to /usr/local/src/." >&2
        exit 1
    fi

    if ! sudo chown -R root:root /usr/local/src/; then
        echo "[ERROR] Failed to change owner of /usr/local/src/." >&2
        exit 1
    fi
}

# Install TA-Lib
install_talib() {
    setup_environment "$1"

    echo "[INFO] Downloading TA-Lib version $VERSION..."
    if ! mkdir install_talib; then
        echo "[ERROR] Failed to create install_talib directory." >&2
        exit 1
    fi
    cd install_talib || exit 1

    if ! curl -L "http://files.id774.net/archive/$FILENAME" -O; then
        echo "[ERROR] Failed to download TA-Lib archive." >&2
        exit 1
    fi

    if [ ! -f "$FILENAME" ]; then
        echo "[ERROR] Downloaded file not found: $FILENAME" >&2
        exit 1
    fi
    echo "[INFO] Download completed: $FILENAME"

    echo "[INFO] Extracting archive: $FILENAME"
    if ! tar xzvf "$FILENAME"; then
        echo "[ERROR] Failed to extract TA-Lib archive." >&2
        exit 1
    fi

    cd ta-lib || exit 1

    echo "[INFO] Building TA-Lib..."
    if ! ./configure --prefix=/usr/local; then
        echo "[ERROR] Configure failed." >&2
        exit 1
    fi

    if ! make; then
        echo "[ERROR] Build failed." >&2
        exit 1
    fi
    echo "[INFO] Build completed successfully."

    echo "[INFO] Installing TA-Lib to /usr/local..."
    if ! sudo make install; then
        echo "[ERROR] Installation failed." >&2
        exit 1
    fi
    echo "[INFO] TA-Lib installed to /usr/local"

    cd .. || exit 1
    [ -n "$2" ] || save_sources

    cd .. || exit 1
    rm -rf install_talib
    echo "[INFO] Installation process completed."
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Perform initial checks
    check_system
    check_commands curl make sudo tar
    check_network
    check_sudo

    # Run the installation process
    install_talib "$1" "$2"

    echo "[INFO] TA-Lib $VERSION installed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

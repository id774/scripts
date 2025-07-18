#!/bin/sh

########################################################################
# install_des.sh: Install and Compile DES Encryption Software
#
#  Description:
#  This script downloads, compiles, and installs the DES encryption software.
#  If the software is not found in the system, it is retrieved from a remote
#  archive, extracted, compiled, and installed.
#  Source files are saved to /usr/local/src/crypt/des unless the -n option is specified.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_des.sh [OPTION]
#
#  Options:
#  -h   Display this help message.
#  -n   Do not save source files after installation.
#
#  Version History:
#  v2.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v2.5 2025-04-27
#       Strengthen error handling during build, installation, and source saving processes.
#  v2.4 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v2.3 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v2.2 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v2.1 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v2.0 2025-03-06
#       Updated to be POSIX compliant.
#       Replaced non-POSIX `which` with `command -v`.
#       Added error handling for directory changes.
#       Integrated command existence and sudo privilege checks.
#       Replaced `md5.sh` with POSIX-compliant `md5sum`.
#       Improved logging with `echo` for status updates.
#  [Intermediate versions omitted for brevity]
#  v1.0 2009-05-21
#       Derived from install_crypt.sh.
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

# Configure environment settings (Linux only)
setup_environment() {
    echo "[INFO] Setting up environment..."
    check_system

    echo "[INFO] Checking system requirements..."
    check_commands dmsetup curl wget md5sum tar make sudo rm mkdir cp chown

    echo "[INFO] Checking network connectivity..."
    check_network

    check_sudo

    OWNER=root:root
}

# Save source files to /usr/local/src/crypt/des
save_sources() {
    echo "[INFO] Saving source files..."

    if [ -d /usr/local/src/crypt/des ]; then
        sudo rm -rf /usr/local/src/crypt/des
    fi

    if ! sudo mkdir -p /usr/local/src/crypt/des; then
        echo "[ERROR] Failed to create /usr/local/src/crypt/des." >&2
        exit 1
    fi

    if ! sudo cp * /usr/local/src/crypt/des; then
        echo "[ERROR] Failed to copy source files to /usr/local/src/crypt/des." >&2
        exit 1
    fi

    sudo chown -R "$OWNER" /usr/local/src/crypt/des

    echo "[INFO] Source files saved."
}

# Install DES encryption software
install_des() {
    echo "[INFO] Setting up DES installation..."
    setup_environment

    mkdir install_des
    cd install_des || exit 1

    echo "[INFO] Downloading software archive..."
    if ! wget http://id774.net/archive/kmdes.tar.gz; then
        echo "[ERROR] Failed to download kmdes.tar.gz." >&2
        exit 1
    fi

    echo "[INFO] Verifying archive integrity..."
    md5sum kmdes.tar.gz

    echo "[INFO] Extracting files..."
    if ! tar xzvf kmdes.tar.gz; then
        echo "[ERROR] Failed to extract kmdes.tar.gz." >&2
        exit 1
    fi

    echo "[INFO] Removing archive..."
    rm -f kmdes.tar.gz

    cd des || exit 1

    if [ -z "$1" ]; then
        save_sources
    fi

    echo "[INFO] Compiling source code..."
    if ! make; then
        echo "[ERROR] Compilation failed." >&2
        exit 1
    fi

    echo "[INFO] Installing compiled binary..."
    if ! sudo make install; then
        echo "[ERROR] Installation failed." >&2
        exit 1
    fi

    echo "[INFO] Cleaning up installation files..."
    cd .. || exit 1
    rm -rf des
    cd .. || exit 1
    rm -rf install_des
    echo "[INFO] DES installation completed."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    if command -v des >/dev/null 2>&1; then
        echo "[INFO] DES is already installed."
    else
        install_des "$@"
    fi
    return 0
}

# Execute main function
main "$@"
exit $?

#!/bin/sh

########################################################################
# install_veracrypt.sh: Install and Configure VeraCrypt
#
#  Description:
#  This script downloads, installs, and configures VeraCrypt for Linux.
#  It ensures that the necessary dependencies and directories exist and
#  properly sets permissions. The script automatically determines the system
#  architecture and installs the predefined version (1.25.9).
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_veracrypt.sh [OPTION]
#
#  Options:
#  -h   Display this help message.
#  -n   Do not save source files after installation.
#
#  Version History:
#  v1.6 2026-02-05
#       Remove preflight network connectivity check.
#  v1.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.4 2025-04-27
#       Add strict error checking for file copy and permission operations.
#  v1.3 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.2 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.1 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.0 2025-03-07
#       Updated to be POSIX compliant.
#       Replaced non-POSIX `which` with `command -v`.
#       Added error handling for directory changes.
#       Integrated command existence and sudo privilege checks.
#       Improved logging with `echo` for status updates.
#       Standardized environment setup and permission handling.
#       Automatically detect system architecture.
#       Fixed VeraCrypt version to 1.25.9 (no longer user-specified).
#       Simplified script by removing version argument.
#  v0.1 2023-01-18
#       Forked from TrueCrypt Installer.
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
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Determine system architecture
get_architecture() {
    case "$(uname -m)" in
        i386|i686)
            ARCH="x86"
            ;;
        x86_64|amd64)
            ARCH="x64"
            ;;
        arm*|aarch64)
            echo "[ERROR] VeraCrypt does not support ARM architectures." >&2
            exit 1
            ;;
        *)
            echo "[ERROR] Unsupported architecture." >&2
            exit 1
            ;;
    esac
}

# Configure environment settings (Linux only)
setup_environment() {
    echo "[INFO] Setting up environment..."
    check_system

    echo "[INFO] Checking system requirements..."
    check_commands dmsetup curl wget tar sudo rm mkdir cp chown file
    check_sudo

    if [ ! -d /usr/local/src/crypt/veracrypt ]; then
        if ! sudo mkdir -p /usr/local/src/crypt/veracrypt; then
            echo "[ERROR] Failed to create /usr/local/src/crypt/veracrypt" >&2
            exit 1
        fi
    fi

    if [ ! -d "$HOME/.tmp" ]; then
        if ! mkdir "$HOME/.tmp"; then
            echo "[ERROR] Failed to create $HOME/.tmp" >&2
            exit 1
        fi
    fi

    if [ "${TMP:-}" != "$HOME/.tmp" ]; then
        echo "[ERROR] TMP environment variable is not set correctly. Expected '$HOME/.tmp', but got '${TMP:-unset}'."
        exit 1
    fi

    OWNER=root:root
    VERSION=1.25.9
}

# Save downloaded packages
save_packages() {
    echo "[INFO] Saving source package to $2."
    if ! sudo cp "$1" "$2"; then
        echo "[ERROR] Failed to copy $1 to $2." >&2
        exit 1
    fi

    if ! sudo chown "$OWNER" "$2/$1"; then
        echo "[ERROR] Failed to change ownership for $2/$1." >&2
        exit 1
    fi
}

# Set proper permissions for VeraCrypt files
set_veracrypt_permission() {
    echo "Setting file permissions..."
    for veracrypt_path in /usr/local/src/crypt/veracrypt /usr/share/veracrypt \
                /usr/share/doc/veracrypt /usr/sbin/mount.veracrypt \
                /usr/local/src/crypt /usr/local/src /usr/bin/veracrypt \
                /usr/bin/veracrypt-uninstall.sh; do
        sudo chown -R "$OWNER" "$veracrypt_path" 2>/dev/null
    done
}

# Install VeraCrypt
install_veracrypt() {
    setup_environment

    get_architecture
    echo "[INFO] Installing VeraCrypt version $VERSION for architecture: $ARCH"
    mkdir install_veracrypt
    cd install_veracrypt || exit 1

    FILE_NAME="veracrypt-$VERSION-setup-console-$ARCH"

    echo "[INFO] Downloading $FILE_NAME..."
    if ! wget "https://files.id774.net/veracrypt/$FILE_NAME"; then
        echo "[ERROR] Failed to download $FILE_NAME." >&2
        exit 1
    fi

    [ -n "$1" ] || save_packages "$FILE_NAME" /usr/local/src/crypt/veracrypt

    chmod +x "./$FILE_NAME"
    if ! "./$FILE_NAME"; then
        echo "[ERROR] Failed to execute installer $FILE_NAME." >&2
        exit 1
    fi

    file /usr/bin/veracrypt
    set_veracrypt_permission

    cd .. || exit 1
    rm -rf install_veracrypt

    echo "[INFO] veracrypt installed successfully."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    install_veracrypt "$@"
    return 0
}

# Execute main function
main "$@"

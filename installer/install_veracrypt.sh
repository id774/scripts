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
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_veracrypt.sh [OPTION] [--yes]
#
#  Options:
#  -h   Display this help message.
#  -n   Do not save source files after installation.
#  --yes  Skip the confirmation prompt (SHA-256 is still displayed).
#         Specify it only as the last argument.
#
#  Notes:
#  - After downloading, the SHA-256 of the file is displayed and y/yes
#    approval is required before it is processed. Any other response, or an
#    unavailable terminal, aborts the installation.
#  - The optional --yes argument skips only the confirmation prompt; the
#    SHA-256 is still displayed. It is accepted only as the last argument.
#
#  Requirements:
#  - The user must have `sha256sum` or `shasum` installed.
#
#  Version History:
#  v1.8 2026-10-21
#       Show downloaded file SHA-256 and confirm before install, with --yes bypass.
#  v1.7 2026-07-11
#       Replace the awk {n,} interval expression in usage() with a portable
#       equivalent, since mawk on some systems matches it incorrectly.
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
#       Make POSIX compliant like install_truecrypt.sh, and pin VeraCrypt to
#       version 1.25.9, removing the now-unused version argument.
#  v0.1 2023-01-18
#       Initial release.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    check_commands awk
    awk '
        BEGIN { in_header = 0 }
        /^#+$/ && length($0) >= 10 { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the system is Linux
check_system() {
    check_commands uname
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
    check_commands sudo
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
    check_commands wget rm mkdir cp chown file uname chmod
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

    show_sha256 "$FILE_NAME"
    [ "$ASSUME_YES" = "true" ] || confirm_installation

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

# Display the SHA-256 of a downloaded file
show_sha256() {
    echo "[INFO] Downloaded file: ${1##*/}"
    if command -v sha256sum >/dev/null 2>&1; then
        sha256_line=$(sha256sum "$1")
    elif command -v shasum >/dev/null 2>&1; then
        sha256_line=$(shasum -a 256 "$1")
    else
        echo "[ERROR] Command 'sha256sum' or 'shasum' is not installed. Please install one of them and try again." >&2
        exit 127
    fi
    sha256_status=$?
    sha256_value=${sha256_line%% *}
    case "$sha256_value" in
        *[!0-9a-f]*) sha256_status=1 ;;
    esac
    if [ "$sha256_status" -ne 0 ] || [ "${#sha256_value}" -ne 64 ]; then
        echo "[ERROR] Failed to calculate SHA-256 of $1." >&2
        exit 1
    fi
    echo "[INFO] SHA-256: $sha256_value"
}

# Ask the user to approve processing of the downloaded file
confirm_installation() {
    printf '[INFO] Proceed with installation? [y/N]: '
    if ! read -r response < /dev/tty; then
        echo >&2
        echo "[ERROR] Failed to read the confirmation from the terminal." >&2
        exit 1
    fi

    case "$response" in
        y|Y|yes|YES) ;;
        *)
            echo "[ERROR] Installation aborted by user." >&2
            exit 1
            ;;
    esac
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Accept --yes only as the last argument and remove it from the arguments
    ASSUME_YES=false
    last_arg=
    for arg in "$@"; do
        last_arg="$arg"
    done
    if [ "$last_arg" = "--yes" ]; then
        ASSUME_YES=true
        i=1
        while [ "$i" -lt "$#" ]; do
            set -- "$@" "$1"
            shift
            i=$((i + 1))
        done
        shift
    fi
    for arg in "$@"; do
        if [ "$arg" = "--yes" ]; then
            usage
        fi
    done

    install_veracrypt "$@"
    return 0
}

# Execute main function
main "$@"

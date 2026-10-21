#!/bin/sh

########################################################################
# install_truecrypt.sh: Install and Configure TrueCrypt 7
#
#  Description:
#  This script downloads, installs, and configures TrueCrypt 7 for Linux.
#  It ensures that the necessary dependencies and directories exist and 
#  properly sets permissions. The script is designed specifically for Linux
#  systems and does not support Windows or macOS.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_truecrypt.sh [VERSION] [OPTION] [--yes]
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
#  v1.9 2026-10-21
#       Show downloaded file SHA-256 and confirm before install, with --yes bypass.
#  v1.8 2026-09-20
#       Apply TrueCrypt ownership changes to each path selected by the permission loop.
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
#       Initial release.
#  [Intermediate versions omitted for brevity]
#  v0.1 2010-08-07
#       Stable release.
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
            echo "[ERROR] TrueCrypt does not support ARM architectures." >&2
            exit 1
            ;;
        *)
            echo "[ERROR] Unsupported architecture." >&2
            exit 1
            ;;
    esac
}

# Validate version input
validate_version() {
    case "$1" in
        "7.0a"|"7.1"|"7.1a")
            VERSION="$1"
            ;;
        "")
            echo "[INFO] No version specified. Defaulting to 7.1a."
            VERSION="7.1a"
            ;;
        *)
            echo "[ERROR] Unsupported TrueCrypt version specified. Allowed versions: 7.0a, 7.1, 7.1a." >&2
            exit 1
            ;;
    esac
}

# Configure environment settings (Linux only)
setup_environment() {
    echo "[INFO] Setting up environment..."
    check_system

    echo "[INFO] Checking system requirements..."
    check_commands wget tar rm mkdir cp chown uname chmod
    check_sudo

    if [ ! -d /usr/local/src/crypt/truecrypt ]; then
        if ! sudo mkdir -p /usr/local/src/crypt/truecrypt; then
            echo "[ERROR] Failed to create /usr/local/src/crypt/truecrypt" >&2
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
}

# Save downloaded packages
save_packages() {
    echo "[INFO] Saving source package to $2"

    if ! sudo cp "$1" "$2"; then
        echo "[ERROR] Failed to copy $1 to $2" >&2
        exit 1
    fi

    if ! sudo chown "$OWNER" "$2/$1"; then
        echo "[ERROR] Failed to change owner of $2/$1" >&2
        exit 1
    fi
}

# Set proper permissions for TrueCrypt files
set_truecrypt_permission() {
    echo "[INFO] Setting file permissions..."
    for truecrypt_path in /usr/local/src/crypt/truecrypt /usr/share/truecrypt \
                /usr/local/src/crypt /usr/local/src /usr/bin/truecrypt \
                /usr/bin/truecrypt-uninstall.sh; do
        if [ -e "$truecrypt_path" ]; then
            sudo chown -R "$OWNER" "$truecrypt_path" 2>/dev/null
        fi
    done
}

# Install TrueCrypt
install_truecrypt() {
    setup_environment

    get_architecture
    validate_version "$1"
    echo "[INFO] Installing TrueCrypt version: $VERSION for architecture: $ARCH"

    mkdir install_truecrypt
    cd install_truecrypt || exit 1

    TAR_NAME="truecrypt-$VERSION-linux-console-$ARCH.tar.gz"
    EXEC_NAME="truecrypt-$VERSION-setup-console-$ARCH"

    echo "[INFO] Downloading $TAR_NAME..."
    if ! wget "https://files.id774.net/truecrypt/$TAR_NAME"; then
        echo "[ERROR] Failed to download $TAR_NAME" >&2
        exit 1
    fi

    show_sha256 "$TAR_NAME"
    [ "$ASSUME_YES" = "true" ] || confirm_installation

    if ! tar xzvf "$TAR_NAME"; then
        echo "[ERROR] Failed to extract $TAR_NAME" >&2
        exit 1
    fi

    [ -n "$2" ] || save_packages "$TAR_NAME" /usr/local/src/crypt/truecrypt

    chmod +x "./$EXEC_NAME"
    if ! "./$EXEC_NAME"; then
        echo "[ERROR] Failed to execute installer $EXEC_NAME" >&2
        exit 1
    fi

    set_truecrypt_permission

    cd .. || exit 1
    rm -rf install_truecrypt

    echo "[INFO] truecrypt installed successfully."
}

# Display the SHA-256 of a downloaded file
show_sha256() {
    echo "[INFO] Downloaded file: ${1##*/}"
    if command -v sha256sum >/dev/null 2>&1; then
        check_commands sha256sum
        sha256_line=$(sha256sum "$1")
    else
        check_commands shasum
        sha256_line=$(shasum -a 256 "$1")
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

    install_truecrypt "$@"
    return 0
}

# Execute main function
main "$@"

#!/bin/sh

########################################################################
# install_safe-chain.sh: Installer for Aikido Safe Chain
#
#  Description:
#  This script downloads a selected Aikido Safe Chain release binary from
#  the official GitHub Release and installs it as bin/safe-chain under an
#  installation prefix on Linux or macOS (x64 or arm64).
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_safe-chain.sh [VERSION [PREFIX [--no-sudo]]]
#
#  VERSION defaults to the version fixed in this script, and PREFIX
#  defaults to /opt/safe-chain/<VERSION>.
#
#  Examples:
#      ./install_safe-chain.sh
#      ./install_safe-chain.sh 1.5.20
#      ./install_safe-chain.sh 1.5.20 /opt/safe-chain/1.5.20
#      ./install_safe-chain.sh 1.5.20 ~/.local/safe-chain/1.5.20 --no-sudo
#
#  Options:
#  -h, --help     Display this help message and exit.
#  -v, --version  Display this help message and exit.
#  --no-sudo      Install without sudo. Use it as the third argument; the
#                 prefix must be writable by the caller.
#
#  Notes:
#  - The default version is a provisional convenience value used when VERSION
#    is omitted. It normally reflects a release selected when this installer
#    was last maintained and is not automatically kept in sync with upstream
#    releases.
#  - An explicit VERSION is downloaded from its GitHub Release using the
#    platform asset name expected by this installer. The corresponding
#    release and asset must exist.
#  - This installer installs only the Safe Chain binary. It does not run Safe
#    Chain, configure shell integration, modify PATH, or change shell
#    configuration. Ordinary package-manager commands are therefore not
#    automatically protected after installation.
#  - Safe Chain writes runtime data under the installation prefix when it
#    runs, so the user running it needs write permission there. This installer
#    does not change prefix ownership or permissions for runtime use.
#  - To enable shell integration, the user may run
#    <PREFIX>/bin/safe-chain setup and then restart the terminal. For CI
#    environments, Safe Chain also provides <PREFIX>/bin/safe-chain setup-ci.
#
#  Requirements:
#  - Linux or macOS on x64 or arm64, with network access to GitHub Releases.
#  - The commands curl, uname, mkdir, cp, chmod, and rm.
#  - Sudo mode: sudo, chown, and sudo privileges.
#
#  Exit Status:
#  0   Installation succeeded, or usage was displayed.
#  1   Unsupported platform, sudo privilege failure, or a failed download,
#      directory, copy, mode, ownership, or cleanup operation.
#  126 A required command exists but is not executable.
#  127 A required command was not found.
#
#  Version History:
#  v1.0 2026-09-25
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
check_sudo_mode() {
    # No-sudo mode must not depend on sudo; validate sudo only after sudo mode is selected.
    [ "$SUDO" = "sudo" ] || return 0
    check_commands sudo chown
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access or specify '--no-sudo'." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    VERSION="${1:-1.5.21}"
    PREFIX="${2:-/opt/safe-chain/$VERSION}"
    if [ "$3" = "--no-sudo" ]; then
        SUDO=""
    else
        SUDO="sudo"
    fi

    SYSTEM="$(uname -s)"
    case "$SYSTEM" in
        Linux)
            OS="linuxstatic"
            OWNER="root:root"
            ;;
        Darwin)
            OS="macos"
            OWNER="root:wheel"
            ;;
        *)
            echo "[ERROR] Unsupported operating system: $SYSTEM" >&2
            exit 1
            ;;
    esac

    MACHINE="$(uname -m)"
    case "$MACHINE" in
        x86_64|amd64)
            ARCH="x64"
            ;;
        aarch64|arm64)
            ARCH="arm64"
            ;;
        *)
            echo "[ERROR] Unsupported architecture: $MACHINE" >&2
            exit 1
            ;;
    esac

    ASSET="safe-chain-$OS-$ARCH"
    TARGET="$PREFIX/bin/safe-chain"
}

# Download and install Safe Chain
install_safe_chain() {
    if ! mkdir install_safe_chain; then
        echo "[ERROR] Failed to create install_safe_chain directory." >&2
        exit 1
    fi

    echo "[INFO] Downloading Aikido Safe Chain $VERSION ($ASSET)."
    if ! curl -fL "https://github.com/AikidoSec/safe-chain/releases/download/$VERSION/$ASSET" -o "install_safe_chain/$ASSET"; then
        echo "[ERROR] Failed to download Aikido Safe Chain $VERSION ($ASSET)." >&2
        exit 1
    fi

    echo "[INFO] Installing Safe Chain to $TARGET."
    if ! $SUDO mkdir -p "$PREFIX/bin"; then
        echo "[ERROR] Failed to create $PREFIX/bin." >&2
        exit 1
    fi
    if ! $SUDO cp "install_safe_chain/$ASSET" "$TARGET"; then
        echo "[ERROR] Failed to copy Safe Chain to $TARGET." >&2
        exit 1
    fi
    if ! $SUDO chmod 0755 "$TARGET"; then
        echo "[ERROR] Failed to set the mode of $TARGET." >&2
        exit 1
    fi

    # Chown only the binary; the rest of the prefix holds runtime data whose
    # ownership the user decides, and a reinstall must not override it.
    if [ "$SUDO" = "sudo" ]; then
        if ! $SUDO chown "$OWNER" "$TARGET"; then
            echo "[ERROR] Failed to change the owner of $TARGET." >&2
            exit 1
        fi
    fi

    # Do not run the installed binary to check it: any invocation, including
    # --version, creates runtime data such as a CA private key under the prefix.

    if ! rm -rf install_safe_chain; then
        echo "[ERROR] Failed to remove install_safe_chain directory." >&2
        exit 1
    fi
}

# Show usage guidance after installation
show_usage_guidance() {
    echo "[INFO] Aikido Safe Chain $VERSION installed to $TARGET."
    echo "[INFO] This installer installs only the Safe Chain binary; it does not run Safe Chain or configure shell integration."
    echo "[INFO] PATH and shell configuration were not changed, so ordinary package-manager commands are not automatically protected."
    echo "[INFO] Safe Chain writes runtime data under $PREFIX when it runs; the user running it needs write permission there."
    echo "[INFO] Runtime ownership and write permission for $PREFIX are not configured by this installer."
    echo "[INFO] To use the installed binary directly:"
    echo "[INFO]   $TARGET npm install <package>"
    echo "[INFO]   $TARGET pip install <package>"
    echo "[INFO] To enable shell integration manually:"
    echo "[INFO]   $TARGET setup"
    echo "[INFO] Restart the terminal after setup for the shell integration to take effect."
    echo "[INFO] For CI environments, Safe Chain also provides:"
    echo "[INFO]   $TARGET setup-ci"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac
    if [ $# -gt 3 ] || { [ $# -eq 3 ] && [ "$3" != "--no-sudo" ]; }; then
        usage
    fi
    case "$1" in
        -*) usage ;;
    esac
    case "$2" in
        -*) usage ;;
    esac

    # Perform initial checks
    check_commands curl uname mkdir cp chmod rm
    setup_environment "$@"
    check_sudo_mode
    install_safe_chain
    show_usage_guidance
    return 0
}

# Execute main function
main "$@"

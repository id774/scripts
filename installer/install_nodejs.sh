#!/bin/sh

########################################################################
# install_nodejs.sh: Installer for Node.js
#
#  Description:
#  This script automates the installation of Node.js by:
#  - Downloading the official binary archive of the specified or default version.
#  - Installing the archive contents to a specified or default location.
#  It does not build Node.js from source.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script without arguments to install the default Node.js version:
#      ./install_nodejs.sh
#
#  Specify a different Node.js version:
#      ./install_nodejs.sh VERSION
#
#  Specify an installation prefix:
#      ./install_nodejs.sh VERSION PREFIX
#
#  Install without sudo (for local user installation):
#      ./install_nodejs.sh VERSION PREFIX --no-sudo
#
#  Options:
#  -h, --help     Display this help message and exit.
#  -v, --version  Display this help message and exit.
#  --no-sudo      Install without sudo. Use it as the third argument,
#                 following the version and the installation prefix.
#
#  Notes:
#  - The default version is a provisional convenience value used when no
#    version is specified. It normally reflects the current release selected
#    when this installer was last maintained and is not automatically kept in
#    sync with upstream releases.
#  - An explicit version is resolved through this installer's configured
#    download source and path rules. It can be installed only when the
#    corresponding artifact is available from that source.
#  - By default, if no installation path is provided, Node.js will be installed under /opt/node/<major>
#  - Linux and macOS on x64 and arm64 are supported.
#  - PATH and global symlinks are not changed.
#
#  Requirements:
#  - Network connectivity is required to download the binary archive.
#  - A supported Linux or macOS environment on x64 or arm64.
#  - Sudo privileges are required for the normal installation with sudo.
#  - The user must have the required commands, including `curl` and `tar`, installed.
#
#  Exit Status:
#  0   Success or usage/help/version display.
#  1   Installation or environment failure.
#  126 Required command exists but is not executable.
#  127 Required command was not found.
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

# Setup version, prefix, sudo mode, and Node.js platform
setup_environment() {
    VERSION="${1:-24.21.0}"
    NODE_MAJOR="$(echo "$VERSION" | awk -F. '{print $1}')"
    PREFIX="${2:-/opt/node/$NODE_MAJOR}"
    if [ "$3" = "--no-sudo" ]; then
        SUDO=""
    else
        SUDO="sudo"
    fi

    case "$(uname -s)" in
        Linux)
            PLATFORM="linux"
            OWNER="root:root"
            ;;
        Darwin)
            PLATFORM="darwin"
            OWNER="root:wheel"
            ;;
        *)
            echo "[ERROR] Unsupported operating system: $(uname -s)" >&2
            exit 1
            ;;
    esac

    case "$(uname -m)" in
        x86_64)
            ARCH="x64"
            ;;
        aarch64|arm64)
            ARCH="arm64"
            ;;
        *)
            echo "[ERROR] Unsupported architecture: $(uname -m)" >&2
            exit 1
            ;;
    esac

    NODE_DIST="node-v$VERSION-$PLATFORM-$ARCH"
}

# Download and install Node.js
install_nodejs() {
    echo "[INFO] Creating temporary build directory."
    if ! mkdir install_nodejs; then
        echo "[ERROR] Failed to create install_nodejs directory." >&2
        exit 1
    fi

    cd install_nodejs || exit 1

    echo "[INFO] Downloading Node.js $VERSION from https://nodejs.org..."
    if ! curl -fL "https://nodejs.org/dist/v$VERSION/$NODE_DIST.tar.gz" -O; then
        echo "[ERROR] Failed to download Node.js archive: $NODE_DIST.tar.gz" >&2
        exit 1
    fi

    echo "[INFO] Extracting archive."
    if ! tar xzf "$NODE_DIST.tar.gz"; then
        echo "[ERROR] Failed to extract Node.js archive." >&2
        exit 1
    fi

    # Return to the invoking directory so that a relative prefix keeps its meaning.
    cd .. || exit 1

    echo "[INFO] Installing Node.js to $PREFIX."
    if ! $SUDO mkdir -p "$PREFIX"; then
        echo "[ERROR] Failed to create $PREFIX." >&2
        exit 1
    fi

    # Copy over the existing prefix, as make install does for Python and Ruby.
    if ! $SUDO cp -pR "install_nodejs/$NODE_DIST/." "$PREFIX"; then
        echo "[ERROR] Failed to copy Node.js files to $PREFIX." >&2
        exit 1
    fi

    if [ "$SUDO" = "sudo" ]; then
        echo "[INFO] Setting ownership of $PREFIX to $OWNER."
        if ! $SUDO chown -R "$OWNER" "$PREFIX"; then
            echo "[ERROR] Failed to change owner of $PREFIX." >&2
            exit 1
        fi
    fi
}

# Verify that the installed Node.js runs and reports the requested version
verify_installation() {
    # A prebuilt binary can fail to start even on a matching OS and architecture.
    echo "[INFO] Verifying installed Node.js."
    INSTALLED_VERSION="$("$PREFIX/bin/node" --version)"
    if [ "$INSTALLED_VERSION" != "v$VERSION" ]; then
        echo "[ERROR] Node.js verification failed: expected v$VERSION, got '$INSTALLED_VERSION'." >&2
        exit 1
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    if [ $# -gt 3 ]; then
        usage
    fi
    if [ $# -eq 3 ] && [ "$3" != "--no-sudo" ]; then
        usage
    fi

    # Perform initial checks
    check_commands curl tar awk mkdir cp rm uname

    # Run the installation process
    setup_environment "$@"
    check_sudo_mode
    install_nodejs
    verify_installation

    echo "[INFO] Cleaning up temporary files."
    if ! rm -rf install_nodejs; then
        echo "[ERROR] Failed to remove temporary build directory." >&2
        exit 1
    fi

    echo "[INFO] Node.js $VERSION installed successfully in $PREFIX."
    return 0
}

# Execute main function
main "$@"

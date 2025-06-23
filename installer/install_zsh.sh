#!/bin/sh

########################################################################
# install_zsh.sh: Installer for Zsh
#
#  Description:
#  This script automates the installation of Zsh by:
#  - Downloading the specified or default version from SourceForge.
#  - Compiling and installing the package.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.4 2025-04-27
#       Add failure checks to Zsh download, build, and installation steps.
#  v1.3 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-20
#       Added network connection check, system validation, command validation, and improved argument handling.
#       Default zsh version 5.9 installs in '/opt/zsh/5.9' directory.
#       Improved directory navigation safety.
#       Set default installation path to /opt/zsh/x.x.
#  [Further version history truncated for brevity]
#  v0.1 2010-09-14
#       First version.
#
#  Usage:
#  Run this script without arguments to install the default version (5.9):
#      ./install_zsh.sh
#  Specify a version to install a different release:
#      ./install_zsh.sh 5.8.1
#  Specify an installation prefix:
#      ./install_zsh.sh 5.8.1 /usr/local
#  Run without sudo (for local installation):
#      ./install_zsh.sh 5.9 ~/.local/zsh --no-sudo
#  Skip saving sources by adding a fourth argument:
#      ./install_zsh.sh 5.9 /opt/zsh sudo -n
#
#  Notes:
#  The current default version for installation is 5.9, but it is anticipated that this version
#  will eventually be moved to the 'old' directory in the future. At that point, to install version
#  5.9, it will need to be explicitly specified. To install the latest version then, the script's
#  default version will need to be updated.
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget`, `make`, `sudo`, and `tar` installed.
#  - Must be executed in a shell environment with internet access.
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
    if [ "$SUDO" = "sudo" ] && ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access or specify 'no-sudo'." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    VERSION="${1:-5.9}"
    MAJOR_MINOR="$(echo "$VERSION" | awk -F. '{print $1"."$2}')"
    PREFIX="${2:-/opt/zsh/$MAJOR_MINOR}"
    if [ -z "$3" ] || [ "$3" = "sudo" ]; then
        SUDO="sudo"
    else
        SUDO=""
    fi
    check_sudo
    DOWNLOAD_SOURCE="${4:-auto}"

    case "$(uname -s)" in
        Darwin)
            OPTIONS="-pR"
            OWNER="root:wheel"
            ;;
        *)
            OPTIONS="-a"
            OWNER="root:root"
            ;;
    esac
}

# Save sources if requested
save_sources() {
    [ "$SUDO" = "sudo" ] || return
    echo "[INFO] Saving source files to /usr/local/src/zsh."

    if ! $SUDO mkdir -p /usr/local/src/zsh; then
        echo "[ERROR] Failed to create /usr/local/src/zsh." >&2
        exit 1
    fi

    if ! $SUDO cp $OPTIONS "zsh-$VERSION" /usr/local/src/zsh; then
        echo "[ERROR] Failed to copy zsh-$VERSION to /usr/local/src/zsh." >&2
        exit 1
    fi

    if ! $SUDO chown $OWNER /usr/local/src/zsh; then
        echo "[ERROR] Failed to change owner of /usr/local/src/zsh." >&2
        exit 1
    fi

    if ! $SUDO chown -R $OWNER /usr/local/src/zsh/zsh-$VERSION; then
        echo "[ERROR] Failed to recursively change owner of zsh-$VERSION." >&2
        exit 1
    fi
}

# Compile and install Zsh
make_and_install() {
    cd "zsh-$VERSION" || exit 1
    if ! ./configure --enable-multibyte --prefix="$PREFIX"; then
        echo "[ERROR] Configure failed." >&2
        exit 1
    fi

    if ! make; then
        echo "[ERROR] Compilation failed." >&2
        exit 1
    fi

    if ! $SUDO make install; then
        echo "[ERROR] Installation failed." >&2
        exit 1
    fi
    cd .. || exit 1
}

# Download and extract Zsh
install_zsh() {
    echo "[INFO] Creating temporary directory for Zsh build."
    if ! mkdir install_zsh; then
        echo "[ERROR] Failed to create install_zsh directory." >&2
        exit 1
    fi

    cd install_zsh || exit 1

    if [ "$USER_SPECIFIED_VERSION" -eq 1 ]; then
        ZSH_URL="https://www.zsh.org/pub/old/zsh-$VERSION.tar.xz"
    else
        ZSH_URL="https://www.zsh.org/pub/zsh-$VERSION.tar.xz"
    fi

    echo "[INFO] Downloading Zsh $VERSION from $ZSH_URL."
    if ! wget "$ZSH_URL"; then
        echo "[ERROR] Failed to download Zsh $VERSION." >&2
        exit 1
    fi

    if [ ! -f "zsh-$VERSION.tar.xz" ]; then
        echo "[ERROR] Downloaded archive not found: zsh-$VERSION.tar.xz" >&2
        exit 1
    fi

    echo "[INFO] Extracting Zsh archive."
    if ! tar xvf "zsh-$VERSION.tar.xz"; then
        echo "[ERROR] Failed to extract Zsh archive." >&2
        exit 1
    fi

    echo "[INFO] Starting build and installation."
    make_and_install

    if [ "$DOWNLOAD_SOURCE" = "auto" ]; then
        save_sources
    fi

    echo "[INFO] Cleaning up temporary build files."
    cd .. || exit 1
    if ! rm -rf install_zsh; then
        echo "[ERROR] Failed to remove temporary directory." >&2
        exit 1
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Perform initial checks
    check_commands curl wget make sudo tar awk mkdir cp chown uname
    check_network

    USER_SPECIFIED_VERSION=0
    if [ -n "$1" ]; then
        USER_SPECIFIED_VERSION=1
    fi

    # Run the installation process
    setup_environment "$@"
    install_zsh "$VERSION" "$PREFIX" "$SUDO" "$4"

    echo "[INFO] Zsh $VERSION installed successfully in $PREFIX."
    return 0
}

# Execute main function
main "$@"
exit $?

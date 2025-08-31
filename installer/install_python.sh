#!/bin/sh

########################################################################
# install_python.sh: Installer for Python
#
#  Description:
#  This script automates the installation of Python by:
#  - Downloading the specified or default version from the official site.
#  - Compiling and installing the package to a specified or default location.
#  - Saving source files only when installing with sudo.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script without arguments to install the default Python version (3.13.7):
#      ./install_python.sh
#
#  Specify a different Python version:
#      ./install_python.sh 3.12.9
#
#  Specify an installation prefix:
#      ./install_python.sh 3.12.9 /opt/python/3.12
#
#  Install without sudo (for local user installation):
#      ./install_python.sh 3.13.7 ~/.local/python --no-sudo
#
#  Notes:
#  - By default, if no installation path is provided, Python will be installed under /opt/python/x.x
#    For example, Python 3.13.7 will be installed to /opt/python/3.13
#
#  - Source files are saved to /usr/local/src/python only when using sudo.
#    If installed without sudo (e.g. using `--no-sudo`), source files will not be saved.
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `curl`, `make`, and `tar` installed.
#  - Must be executed in a shell environment with internet access.
#
#  Version History:
#  v2.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v2.4 2025-04-27
#       Add failure checks to Python build and installation steps.
#  v2.3 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v2.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v2.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v2.0 2025-03-19
#       Added network connection check, system validation, command validation, and improved argument handling.
#       Default Python version 3.13.7 installs in '/opt/python/3.13' directory.
#       Improved directory navigation safety.
#       Set default installation path to /opt/python/x.x.
#  [Further version history truncated for brevity]
#  v1.0 2009-01-07
#       First stable release.
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
    if [ "$SUDO" = "sudo" ] && ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access or specify 'no-sudo'." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    VERSION="${1:-3.13.7}"
    MAJOR_MINOR="$(echo "$VERSION" | awk -F. '{print $1"."$2}')"
    PREFIX="${2:-/opt/python/$MAJOR_MINOR}"
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
    echo "[INFO] Saving source files to /usr/local/src/python."

    if ! $SUDO mkdir -p /usr/local/src/python; then
        echo "[ERROR] Failed to create /usr/local/src/python." >&2
        exit 1
    fi

    if ! $SUDO cp $OPTIONS "Python-$VERSION" /usr/local/src/python; then
        echo "[ERROR] Failed to copy source files to /usr/local/src/python." >&2
        exit 1
    fi

    if ! $SUDO chown $OWNER /usr/local/src/python; then
        echo "[ERROR] Failed to change owner of /usr/local/src/python." >&2
        exit 1
    fi

    if ! $SUDO chown -R $OWNER /usr/local/src/python/Python-$VERSION; then
        echo "[ERROR] Failed to change owner recursively for Python-$VERSION." >&2
        exit 1
    fi
}

# Compile and install Python
make_and_install() {
    echo "[INFO] Configuring build..."
    cd "Python-$VERSION" || exit 1
    if ! ./configure --prefix="$PREFIX"; then
        echo "[ERROR] Configure failed." >&2
        exit 1
    fi

    echo "[INFO] Compiling source..."
    if ! make; then
        echo "[ERROR] Compilation failed." >&2
        exit 1
    fi

    echo "[INFO] Installing Python..."
    if ! $SUDO make install; then
        echo "[ERROR] Installation failed." >&2
        exit 1
    fi
    cd .. || exit 1
}

# Download and install Python
install_python() {
    echo "[INFO] Creating temporary build directory."
    if ! mkdir install_python; then
        echo "[ERROR] Failed to create build directory." >&2
        exit 1
    fi

    cd install_python || exit 1

    echo "[INFO] Downloading Python $VERSION from python.org..."
    if ! curl -L "http://www.python.org/ftp/python/$VERSION/Python-$VERSION.tgz" -O; then
        echo "[ERROR] Failed to download Python archive." >&2
        exit 1
    fi

    if [ ! -f "Python-$VERSION.tgz" ]; then
        echo "[ERROR] Downloaded archive not found: Python-$VERSION.tgz" >&2
        exit 1
    fi

    echo "[INFO] Extracting archive..."
    if ! tar xzvf "Python-$VERSION.tgz"; then
        echo "[ERROR] Failed to extract archive." >&2
        exit 1
    fi

    make_and_install

    if [ "$DOWNLOAD_SOURCE" = "auto" ]; then
        save_sources
    fi

    echo "[INFO] Cleaning up temporary files."
    cd .. || exit 1
    if ! $SUDO rm -rf install_python; then
        echo "[ERROR] Failed to remove temporary directory." >&2
        exit 1
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Perform initial checks
    check_commands curl sudo make tar awk mkdir cp chown uname
    check_network

    # Run the installation process
    setup_environment "$@"
    install_python

    echo "[INFO] Python $VERSION installed successfully in $PREFIX."
    return 0
}

# Execute main function
main "$@"
exit $?

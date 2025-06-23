#!/bin/sh

########################################################################
# install_ruby.sh: Installer for Ruby
#
#  Description:
#  This script automates the installation of Ruby by:
#  - Downloading the specified or default version from the official site.
#  - Compiling and installing the package.
#  - Optionally saving the source files for future use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v3.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v3.4 2025-04-27
#       Add failure checks to Ruby download, build, and installation steps.
#  v3.3 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v3.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v3.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v3.0 2025-03-19
#       Added network connection check, system validation, command validation, and improved argument handling.
#       Default Ruby version 3.4.4 installs in '/opt/ruby/3.4' directory.
#       Improved directory navigation safety.
#       Set default installation path to /opt/ruby/x.x.
#  [Further version history truncated for brevity]
#  v1.0 2008-06-23
#       First stable release.
#
#  Usage:
#  Run this script without arguments to install the default Ruby version (3.4.4):
#      ./install_ruby.sh
#  Specify a different Ruby version:
#      ./install_ruby.sh 3.3.7
#  Specify an installation prefix:
#      ./install_ruby.sh 3.3.7 /opt/ruby/3.3
#  Run without sudo (for local installation):
#      ./install_ruby.sh 3.4.4 ~/.local/ruby --no-sudo
#  Skip saving sources by adding a fourth argument:
#      ./install_ruby.sh 3.4.4 /opt/ruby sudo -n
#
#  By default, if no installation path is provided, the script will install Ruby under /opt/ruby/x.x.
#  For example, Ruby 3.4.4 will be installed in /opt/ruby/3.4.
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `curl`, `make`, and `tar` installed.
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
    VERSION="${1:-3.4.4}"
    RUBY_MAJOR="$(echo "$VERSION" | awk -F. '{print $1"."$2}')"
    PREFIX="${2:-/opt/ruby/$RUBY_MAJOR}"
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
    echo "[INFO] Saving source files to /usr/local/src/ruby."

    if ! $SUDO mkdir -p /usr/local/src/ruby; then
        echo "[ERROR] Failed to create /usr/local/src/ruby." >&2
        exit 1
    fi

    if ! $SUDO cp -a "ruby-$VERSION" /usr/local/src/ruby/; then
        echo "[ERROR] Failed to copy ruby-$VERSION to /usr/local/src/ruby/." >&2
        exit 1
    fi

    if ! $SUDO chown $OWNER /usr/local/src/ruby; then
        echo "[ERROR] Failed to change owner of /usr/local/src/ruby." >&2
        exit 1
    fi

    if ! $SUDO chown -R $OWNER /usr/local/src/ruby/ruby-$VERSION; then
        echo "[ERROR] Failed to recursively change owner of ruby-$VERSION." >&2
        exit 1
    fi
}

# Compile and install Ruby
make_and_install() {
    echo "[INFO] Configuring the build..."
    cd "ruby-$VERSION" || exit 1
    if ! ./configure --prefix="$PREFIX"; then
        echo "[ERROR] Configure failed." >&2
        exit 1
    fi

    echo "[INFO] Building Ruby..."
    if ! make; then
        echo "[ERROR] Build failed." >&2
        exit 1
    fi

    echo "[INFO] Installing Ruby to $PREFIX..."
    if ! $SUDO make install; then
        echo "[ERROR] Installation failed." >&2
        exit 1
    fi
    cd .. || exit 1
}

# Download and install Ruby
install_ruby() {
    echo "[INFO] Creating temporary build directory."
    if ! mkdir install_ruby; then
        echo "[ERROR] Failed to create install_ruby directory." >&2
        exit 1
    fi

    cd install_ruby || exit 1

    echo "[INFO] Downloading Ruby $VERSION from https://cache.ruby-lang.org..."
    if ! curl -L "https://cache.ruby-lang.org/pub/ruby/$RUBY_MAJOR/ruby-$VERSION.tar.gz" -O; then
        echo "[ERROR] Failed to download Ruby archive." >&2
        exit 1
    fi

    if [ ! -f "ruby-$VERSION.tar.gz" ]; then
        echo "[ERROR] Downloaded archive not found: ruby-$VERSION.tar.gz" >&2
        exit 1
    fi

    echo "[INFO] Extracting archive."
    if ! tar xzvf "ruby-$VERSION.tar.gz"; then
        echo "[ERROR] Failed to extract Ruby archive." >&2
        exit 1
    fi

    make_and_install

    if [ "$DOWNLOAD_SOURCE" = "auto" ]; then
        save_sources
    fi

    cd .. || exit 1
    if ! $SUDO rm -rf install_ruby; then
        echo "[ERROR] Failed to remove temporary build directory." >&2
        exit 1
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Perform initial checks
    check_commands curl sudo make tar awk mkdir cp chown uname
    check_network

    # Run the installation process
    setup_environment "$@"
    install_ruby

    echo "[INFO] Ruby $VERSION installed successfully in $PREFIX."
    return 0
}

# Execute main function
main "$@"
exit $?

#!/bin/sh

########################################################################
# install_ruby.sh: Installer for Ruby
#
#  Description:
#  This script automates the installation of Ruby by:
#  - Downloading the specified version from the official site.
#  - Compiling and installing the package.
#  - Optionally saving the source files for future use.
#  - Compiling and installing essential extensions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v3.0 2025-03-19
#       Restored Ruby installation process from official sources.
#       Default Ruby version 3.4.2 installs in '/opt/ruby/3.4' directory.
#       Improved directory navigation safety.
#       Consolidated previous modifications under a single version entry.
#       Set default installation path to /opt/ruby/x.x.
#  v2.5 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v2.4 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v2.3 2023-03-12
#       Remove obsolete versions.
#  v2.2 2019-01-14
#       Remove obsolete versions.
#  v2.1 2015-03-11
#       Fix bugs.
#       Specify nosudo option.
#  v1.0 2008-06-23
#       Stable.
#
#  Usage:
#  Run this script without arguments to install the default Ruby version (3.4.2):
#      ./install_ruby.sh
#  Specify a different Ruby version:
#      ./install_ruby.sh 3.3.7
#  Specify an installation prefix:
#      ./install_ruby.sh 3.3.7 /opt/ruby/3.3
#  Run without sudo (for local installation):
#      ./install_ruby.sh 3.3.7 ~/.local/ruby no-sudo
#  Skip saving sources by adding a third argument:
#      ./install_ruby.sh 3.3.7 /opt/ruby sudo -n
#
#  By default, if no installation path is provided, the script will install Ruby under /opt/ruby/x.x.
#  For example, Ruby 3.4.2 will be installed in /opt/ruby/3.4.
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `curl`, `make`, and `tar` installed.
#  - Must be executed in a shell environment with internet access.
#
########################################################################

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to check network connectivity
check_network() {
    if ! curl -s --head --connect-timeout 5 http://clients3.google.com/generate_204 >/dev/null; then
        echo "Error: No network connection detected. Please check your internet access." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Setup version and environment
setup_environment() {
    RUBY_VERSION="${1:-3.4.2}"
    RUBY_MAJOR="$(echo "$RUBY_VERSION" | awk -F. '{print $1"."$2}')"
    PREFIX="${2:-/opt/ruby/$RUBY_MAJOR}"

    if [ "$3" = "no-sudo" ]; then
        SUDO=""
    else
        SUDO="sudo"
    fi
    [ "$SUDO" = "sudo" ] && check_sudo
}

# Save sources if requested
save_sources() {
    [ -n "$4" ] && return
    [ "$SUDO" = "sudo" ] || return
    $SUDO mkdir -p /usr/local/src/ruby
    $SUDO cp -a "ruby-$RUBY_VERSION" /usr/local/src/ruby/
    $SUDO chown -R root:root /usr/local/src/ruby/ruby-$RUBY_VERSION
}

# Download and install Ruby
install_ruby() {
    mkdir install_ruby
    cd install_ruby || exit 1
    curl -L "https://cache.ruby-lang.org/pub/ruby/$RUBY_MAJOR/ruby-$RUBY_VERSION.tar.gz" -O
    tar xzvf "ruby-$RUBY_VERSION.tar.gz"
    cd "ruby-$RUBY_VERSION" || exit 1
    ./configure --prefix="$PREFIX"
    make
    $SUDO make install
    cd ../.. || exit 1
    save_sources "$RUBY_VERSION" "$PREFIX" "$SUDO" "$4"
    rm -rf install_ruby
}

# Main function to execute the script
main() {
    # Perform initial checks
    check_commands curl sudo make tar awk mkdir cp chown
    check_network

    # Run the installation process
    setup_environment "$1" "$2" "$3" "$4"
    install_ruby "$RUBY_VERSION" "$PREFIX" "$SUDO" "$4"

    echo "Ruby $RUBY_VERSION installed successfully in $PREFIX."
}

# Execute main function
main "$@"

#!/bin/sh

########################################################################
# install_mecab_ruby.sh: Installer for MeCab Ruby Binding
#
#  Description:
#  This script automates the installation of the MeCab Ruby binding by:
#  - Setting up the Ruby environment.
#  - Compiling and installing the MeCab Ruby binding.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-16
#       Unified structure, added system checks, improved error handling.
#  v0.1 2014-02-09
#       First.
#
#  Usage:
#  Run this script without arguments to install with default settings:
#      ./install_mecab_ruby.sh
#  Specify a Ruby path and source path:
#      ./install_mecab_ruby.sh /opt/ruby/current /usr/local/src/mecab/mecab-ruby-0.994
#  Disable sudo by passing any third argument (e.g., '-n', 'nosudo'):
#      ./install_mecab_ruby.sh /opt/ruby/current /usr/local/src/mecab/mecab-ruby-0.994 -n
#
#  Requirements:
#  - The user must have `ruby` installed and accessible.
#  - This script is intended for Linux systems only.
#
########################################################################

# Function to check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "Error: This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to check network connectivity
check_network() {
    if ! ping -c 1 id774.net >/dev/null 2>&1; then
        echo "Error: No network connection detected. Please check your internet access." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Setup environment
setup_environment() {
    RUBY="${1:-/opt/ruby/current}/bin/ruby"
    TARGET="${2:-/usr/local/src/mecab/mecab-ruby-0.994}"
    SUDO="sudo"
    [ -n "$3" ] && SUDO=""
    [ "$SUDO" = "sudo" ] && check_sudo
}

# Compile and install MeCab Ruby binding
source_compile() {
    cd "$TARGET" || exit 1
    $SUDO "$RUBY" extconf.rb
    $SUDO make
    $SUDO make install
}

# Main execution function
main() {
    check_system
    check_commands ruby
    check_network
    setup_environment "$@"
    source_compile
    echo "MeCab Ruby binding installed successfully."
}

main "$@"

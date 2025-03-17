#!/bin/sh

########################################################################
# debian_xulext.sh: Install XUL extensions on Debian-based systems
#
#  Description:
#  This script installs various XUL extensions for Firefox on Debian-based systems
#  while ensuring proper system compatibility and package availability.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-13
#       Ensured POSIX compliance and improved error handling.
#       Added system compatibility check for Debian.
#       Enhanced package installation logic.
#       Redirected error messages to stderr for better logging and debugging.
#  [Further version history truncated for brevity]
#  v0.1 2015-03-19
#       First version.
#
#  Usage:
#  Run the script directly:
#      ./debian_xulext.sh
#  This script checks if XUL extensions are installed and installs missing ones.
#
#  Notes:
#  - The script is designed for Debian-based systems (Debian, Ubuntu, etc.).
#  - Internet connectivity is required for package installation.
#  - Review and modify the package list as needed before execution.
#
#  Error Conditions:
#  - If the system is not Debian-based, the script exits with an error.
#  - If required commands are missing, execution is halted.
#  - Errors from apt-get should be resolved based on their output.
#
########################################################################

# Check if the system supports apt-get
check_environment() {
    if ! command -v apt-get >/dev/null 2>&1; then
        echo "Error: apt-get is not available on this system. This script requires a Debian-based environment." >&2
        exit 1
    fi
}

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

# Install package if not already installed
smart_apt() {
    for pkg in "$@"; do
        if ! dpkg-query -W -f='${Status}' "$pkg" 2>/dev/null | grep -q "ok installed"; then
            echo "Installing $pkg..."
            sudo apt-get -y install "$pkg"
        else
            echo "$pkg is already installed. Skipping."
        fi
    done
}

# XUL extension packages
xulext_packages() {
    smart_apt \
      xul-ext-adblock-plus \
      xul-ext-firebug \
      xul-ext-greasemonkey \
      xul-ext-noscript \
      xul-ext-scrapbook \
      xul-ext-toggle-proxy \
      xul-ext-useragentswitcher \
      xul-ext-webdeveloper
}

# Main function to execute the script
main() {
    check_environment
    check_commands sudo apt-get dpkg-query grep
    xulext_packages
    echo "XUL extensions installation completed."
}

# Execute main function
main "$@"

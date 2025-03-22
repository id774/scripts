#!/bin/sh

########################################################################
# setup_xdg_dirs_en.sh: User Directory Management Script for GNU/Linux
#
#  Description:
#  This script manages user directories on GNU/Linux systems. It installs
#  and configures xdg-user-dirs-gtk for managing user directories like
#  Documents, Downloads, Music, etc., and allows customization of these
#  directory paths.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.3 2025-03-19
#       Integrated system requirement checks.
#       Removed vim execution, now instructs the user to manually edit the config file.
#  v1.2 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.1 2023-11-30
#       Updated to check if the environment is GNU/Linux and if 'apt' is available.
#       Changed package installation command from 'apt-get' to 'apt'.
#  v1.0 2010-07-27
#       Initial release. Basic functionality for managing user directories.
#
#  Usage:
#  Run the script without any arguments:
#      ./setup_xdg_dirs_en.sh
#
########################################################################

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}


# Install xdg-user-dirs-gtk if not already installed
install_xdg_user_dirs_gtk() {
    if ! dpkg -l | grep -q xdg-user-dirs-gtk; then
        echo "Installing xdg-user-dirs-gtk..."
        sudo apt install -y xdg-user-dirs-gtk
        if [ $? -ne 0 ]; then
            echo "Error: Failed to install xdg-user-dirs-gtk." >&2
            exit 1
        fi
    else
        echo "xdg-user-dirs-gtk is already installed."
    fi
}

# Update user directories and check for success
update_xdg_dirs() {
    echo "Updating XDG user directories..."
    LANG=C xdg-user-dirs-gtk-update
    if [ $? -eq 0 ]; then
        echo "XDG user directories have been successfully updated."
    else
        echo "Error: Failed to update XDG user directories." >&2
        exit 1
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    # Perform system checks
    check_system
    check_commands apt sudo dpkg grep
    check_sudo

    # Install xdg-user-dirs-gtk if necessary
    install_xdg_user_dirs_gtk

    # Update user directories
    update_xdg_dirs

    # Notify the user to manually edit the configuration file if needed
    echo "If you need to customize your directories, edit the file manually:"
    echo "    $HOME/.config/user-dirs.dirs"
    echo "Then, log out and log back in for changes to take effect."
}

# Execute main function
main "$@"

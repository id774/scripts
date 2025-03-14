#!/bin/sh

########################################################################
# debian_desktop_apt.sh: Bulk Apt Install Script for Debian Desktop
#
#  Description:
#  This script automates the installation of various packages for a Debian
#  desktop environment. It is designed to efficiently install a set of
#  pre-defined packages, ensuring that only the necessary packages are
#  installed and avoiding redundant installations.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-03-13
#       Added system checks and improved error handling.
#       Redirected error messages to stderr for better logging and debugging.
#  v1.1 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.0 2024-03-23
#       Refactored for improved structure and POSIX compliance.
#  v0.1 2011-09-28
#       Forked from Initial Setup Script.
#
#  Usage:
#  Run the script directly without explicit 'sudo', as it uses 'sudo' internally for privilege elevation:
#      ./debian_desktop_apt.sh
#  You may be prompted to enter your password due to 'sudo' commands within the script. Ensure that you
#  trust the script before executing it, as it performs system updates, upgrades, and installs a pre-defined
#  set of packages, including basic tools, system utilities, development tools, editors, and more.
#
#  Notes:
#  - The script is designed for Debian-based desktop systems.
#  - Internet connectivity is required for package downloads.
#  - Review and modify the package lists as needed for your setup.
#
#  Error Conditions:
#  The script checks if each package is already installed to prevent unnecessary reinstallation.
#  However, it does not explicitly handle errors such as package unavailability or network issues.
#  These should be resolved based on the output of the apt-get command.
#
########################################################################

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

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
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Check if a desktop environment is installed
check_desktop_installed() {
    if tasksel --list-tasks | grep -q '^i.*desktop'; then
        echo "Desktop environment detected."
    else
        echo "No desktop environment found. Please install a desktop environment before running this script." >&2
        exit 1
    fi
}

# System update and upgrade
apt_upgrade() {
    sudo apt-get update &&
    sudo apt-get -y upgrade &&
    sudo apt-get autoclean &&
    sudo apt-get -y autoremove
}

# Install package if not already installed
smart_apt() {
    for pkg do
        if [ "$(dpkg-query -W -f='${Status}' "$pkg" 2>/dev/null | grep -c "ok installed")" -eq 0 ]; then
            sudo apt-get -y install "$pkg"
        fi
    done
}

# Desktop environment packages
desktop_environment() {
    smart_apt xfwm4 xfwm4-themes xfce4-goodies xfce4-terminal gnome-themes gnome-themes-extras
}

# Font packages
fonts_packages() {
    smart_apt xfonts-mplus xfonts-shinonome ttf-bitstream-vera fonts-vlgothic fonts-ipafont
}

# Package manager tools
package_manager() {
    smart_apt synaptic gdebi
}

# Multimedia codec packages
codec_packages() {
    smart_apt gstreamer0.10-ffmpeg
}

# Icon packages
icon_packages() {
    smart_apt ubuntu-artwork xubuntu-artwork human-icon-theme
}

# Configuration tools
gconf_packages() {
    smart_apt gconf-editor dconf-tools gnome-tweak-tool
}

# Utility packages
utils_packages() {
    smart_apt ranger caca-utils highlight atool w3m poppler-utils mediainfo
}

# Optional packages
optional_packages() {
    smart_apt thunderbird thunderbird-locale-ja firefox firefox-locale-ja \
              fcitx-mozc emacs-mozc libreoffice vim-gnome gthumb thunar bittorrent-gui ktorrent qbittorrent \
              vlc pidgin xpdf evince comix fbreader wireshark xtightvncviewer chromium chromium-l10n
}

# Main operation
main() {
    check_environment
    check_commands sudo dpkg-query grep tasksel
    check_desktop_installed
    check_sudo
    apt_upgrade
    desktop_environment
    fonts_packages
    package_manager
    codec_packages
    icon_packages
    gconf_packages
    utils_packages
    optional_packages
}

main "$@"

#!/bin/sh

########################################################################
# debian_desktop_apt.sh: Bulk Apt Install Script for Debian Desktop
#
#  Description:
#  Install and update desktop software on Debian-based systems, grouped by purpose.
#  Package categories (representative examples):
#    - Desktop environment:
#        * Xfce components and themes: xfwm4, xfwm4-themes, xfce4-goodies, xfce4-terminal
#        * GNOME themes: gnome-themes, gnome-themes-extras
#    - Japanese input method:
#        * fcitx5, fcitx5-mozc, fcitx5-config-qt, fcitx5-configtool
#    - Fonts:
#        * xfonts-mplus, xfonts-shinonome, ttf-bitstream-vera, fonts-vlgothic, fonts-ipafont
#    - GUI package managers:
#        * synaptic, gdebi
#    - Multimedia codecs:
#        * gstreamer1.0-libav
#        * (optional) proprietary codecs or extras as needed
#    - Icon and artwork themes:
#        * ubuntu-artwork, xubuntu-artwork, human-icon-theme
#    - Desktop configuration tools:
#        * dconf-cli, gnome-tweaks
#    - Utilities:
#        * ranger, atool, highlight, caca-utils, w3m, poppler-utils, mediainfo
#    - Optional desktop applications:
#        * Browsers: firefox, firefox-locale-ja, chromium, chromium-l10n
#        * Mail: thunderbird, thunderbird-locale-ja
#        * Office: libreoffice
#        * Editors and IME helpers: fcitx5-mozc, emacs-mozc, mozc-server, mozc-utils-gui
#        * File and image tools: thunar, gthumb
#        * Media and comms: vlc, pidgin
#        * Documents and readers: xpdf, evince, comix, fbreader
#        * Networking and remote: wireshark, xtightvncviewer
#        * Torrents: bittorrent-gui, ktorrent, qbittorrent
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script directly without explicit 'sudo', as it uses 'sudo' internally for privilege elevation:
#      ./debian_desktop_apt.sh
#
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
#  Version History:
#  v2.1 2025-08-20
#       Expanded header documentation to list package categories and representative
#       software installed via apt for improved clarity.
#  v2.0 2025-08-12
#       Improved desktop environment detection logic and reviewed several package selections.
#  v1.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.4 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.3 2025-03-22
#       Unify usage information by extracting help text from header comments.
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

# Check if the system is Linux
check_system() {
    os="$(uname -s 2>/dev/null)"
    if [ "$os" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
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

# Check if a desktop environment is installed (Debian/Ubuntu)
check_desktop_installed() {
    # Prefer tasksel if available
    if command -v tasksel >/dev/null 2>&1; then
        if LC_ALL=C tasksel --list-tasks | grep -q '^i.*desktop'; then
            echo "[INFO] Desktop environment detected via tasksel."
            return 0
        fi
    fi

    # Fallback: check for session files
    if [ -d /usr/share/xsessions ] && ls /usr/share/xsessions/*.desktop >/dev/null 2>&1; then
        echo "[INFO] Desktop environment detected via /usr/share/xsessions."
        return 0
    fi
    if [ -d /usr/share/wayland-sessions ] && ls /usr/share/wayland-sessions/*.desktop >/dev/null 2>&1; then
        echo "[INFO] Desktop environment detected via /usr/share/wayland-sessions."
        return 0
    fi

    echo "[ERROR] No desktop environment detected." >&2
    exit 1
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

# Ja inputs packages
inputs_packages() {
    smart_apt fcitx5 fcitx5-mozc fcitx5-config-qt fcitx5-configtool
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
    smart_apt gstreamer1.0-libav
    # Optional: proprietary codecs, fonts, etc.
    # smart_apt ubuntu-restricted-extras
}

# Icon packages
icon_packages() {
    smart_apt ubuntu-artwork xubuntu-artwork human-icon-theme
}

# Configuration tools
gconf_packages() {
    smart_apt dconf-cli gnome-tweaks
}

# Utility packages
utils_packages() {
    smart_apt ranger caca-utils highlight atool w3m poppler-utils mediainfo
}

# Remove Desktop client
remote_desktop_packages() {
    smart_apt remmina remmina-plugin-rdp remmina-plugin-vnc
}

# Optional packages
optional_packages() {
    smart_apt thunderbird thunderbird-locale-ja firefox firefox-locale-ja \
              fcitx5-mozc emacs-mozc mozc-server mozc-utils-gui libreoffice gthumb thunar bittorrent-gui ktorrent qbittorrent \
              vlc pidgin xpdf evince comix fbreader wireshark xtightvncviewer chromium chromium-l10n
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_desktop_installed
    check_commands sudo apt-get dpkg-query grep tasksel
    check_sudo
    apt_upgrade
    desktop_environment
    inputs_packages
    fonts_packages
    package_manager
    codec_packages
    icon_packages
    gconf_packages
    utils_packages
    remote_desktop_packages
    optional_packages

    echo "[INFO] All specified desktop packages have been installed."
    return 0
}

# Execute main function
main "$@"

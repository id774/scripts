#!/bin/sh

########################################################################
# debian_setup.sh: Debian batch setup script
#
#  Description:
#  This script automates the installation and configuration of essential
#  system utilities, dotfiles, cryptographic tools, and system security
#  settings on Debian-based systems. It ensures a consistent setup
#  for system administration and development environments.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-13
#       Enhanced documentation and comments for better maintainability.
#       Ensured strict POSIX compliance.
#       Added system compatibility check for Linux.
#       Redirected error messages to stderr for better logging and debugging.
#       Optimize zsh setup, cryptographic tool installation.
#  [Further version history truncated for brevity]
#  v0.1 2011-09-28
#       First version.
#
#  Usage:
#  Run the script directly:
#      ./debian_setup.sh
#  Ensure that required setup scripts exist in the designated directory.
#  This script verifies system compatibility and necessary privileges before execution.
#
#  Notes:
#  - The script is designed for Debian-based systems (Debian, Ubuntu, etc.).
#  - Internet connectivity is required for downloading dotfiles and system utilities.
#  - Review and modify the installation scripts as needed before execution.
#
#  Error Conditions:
#  - If the system is not Linux, the script exits with an error.
#  - If required commands are missing, the script exits with an error.
#  - If the user lacks sudo privileges, execution is halted.
#  - Errors from underlying scripts should be resolved based on their output.
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

# Function to verify script environment
setup_environment() {
    SCRIPTS="$HOME/scripts"
    if [ ! -d "$SCRIPTS" ]; then
        echo "Error: Directory '$SCRIPTS' does not exist. Please create it or specify the correct path." >&2
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

# Set zsh as the default shell for the user and root
set_zsh_to_default() {
    if [ "$(getent passwd "$USER" | cut -d: -f7)" != "/bin/zsh" ]; then
        chsh -s /bin/zsh
    fi

    if [ "$(getent passwd root | cut -d: -f7)" != "/bin/zsh" ]; then
        sudo chsh -s /bin/zsh root
    fi
}

# Install various dotfiles configurations
install_dot_vim() {
    sudo locale-gen ja_JP.UTF-8
    "$SCRIPTS/installer/install_dotvim.sh"
}

install_dot_zsh() {
    test -d "$HOME/local/github" || mkdir -p "$HOME/local/github"
    cd "$HOME/local/github"
    git clone https://github.com/id774/dot_zsh.git
    cd
    ln -s "$HOME/local/github/dot_zsh"
    "$HOME/local/github/dot_zsh/install_dotzsh.sh"
}

install_dot_emacs() {
    test -d "$HOME/local/github" || mkdir -p "$HOME/local/github"
    cd "$HOME/local/github"
    git clone https://github.com/id774/dot_emacs.git
    cd
    ln -s "$HOME/local/github/dot_emacs"
    "$HOME/local/github/dot_emacs/install_dotemacs.sh"
}

install_dot_files() {
    "$SCRIPTS/installer/install_dotfiles.sh"
}

# Install cryptographic tools
install_crypt() {
    if ! command -v des >/dev/null 2>&1; then
        "$SCRIPTS/installer/install_des.sh"
    fi

    if ! command -v truecrypt >/dev/null 2>&1; then
        "$SCRIPTS/installer/install_truecrypt.sh"
    fi

    if ! command -v veracrypt >/dev/null 2>&1; then
        "$SCRIPTS/installer/install_veracrypt.sh"
    fi
}

# Setup various system utilities
setup_sysadmin_scripts() {
    "$SCRIPTS/installer/setup_sysadmin_scripts.sh" uninstall
    "$SCRIPTS/installer/setup_sysadmin_scripts.sh" install
}

setup_get_resources() {
    "$SCRIPTS/installer/install_get_resources.sh"
}

setup_munin() {
    "$SCRIPTS/installer/install_munin.sh"
}

setup_securetty() {
    "$SCRIPTS/securetty.sh"
}

# Set permissions for /usr/src
permission_for_src() {
    sudo chown -R root:root /usr/src
    sudo chown -R root:root /usr/local/src
}

# Configure system settings
configure_sysctl() {
    "$SCRIPTS/installer/configure_sysctl.sh" --apply
}

configure_sysstat() {
    sudo dpkg-reconfigure sysstat
}

configure_hddtemp() {
    sudo dpkg-reconfigure hddtemp
}

# Erase history files
erase_history() {
    test -f "$HOME/.bash_history" && sudo rm "$HOME/.bash_history"
    test -f "$HOME/.mysql_history" && sudo rm "$HOME/.mysql_history"
    test -f "$HOME/.viminfo" && sudo rm "$HOME/.viminfo"
}

# Main operation function
main() {
    check_system
    setup_environment
    check_commands sudo vi zsh git cut getent dpkg-reconfigure
    check_sudo
    set_zsh_to_default
    test -d "$HOME/.vim" || install_dot_vim
    test -d "$HOME/local/github/dot_zsh" || install_dot_zsh
    test -d "$HOME/local/github/dot_emacs" || install_dot_emacs
    install_dot_files
    install_crypt
    setup_sysadmin_scripts
    setup_get_resources
    setup_munin
    setup_securetty
    permission_for_src
    configure_sysctl
    configure_sysstat
    configure_hddtemp
    erase_history
}

# Execute main operations
main "$@"

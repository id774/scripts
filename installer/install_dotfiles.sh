#!/bin/sh

########################################################################
# install_dotfiles.sh: Deploy and Configure User Dotfiles
#
#  Description:
#  This script automates the deployment and configuration of user dotfiles
#  across various environments, including macOS, Linux, and other Unix-like systems.
#  It ensures consistent user configurations by copying predefined dotfiles
#  to appropriate locations, managing permissions, and applying necessary settings.
#
#  Features:
#  - Deploys dotfiles such as .zshrc, .vimrc, .gitconfig, .emacs, etc.
#  - Ensures required directories exist and have correct permissions.
#  - Configures Emacs settings, including site-lisp and backup directories.
#  - Supports multiple users on Linux/macOS.
#  - Uses POSIX-compliant scripting practices for better compatibility.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.2 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#       Optimize home directory ownership handling.
#  v2.1 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v2.0 2025-03-04
#       Improved POSIX compliance by quoting variables and using safer constructs.
#       Ensured robust directory creation and permission handling.
#       Streamlined deployment logic for better maintainability.
#  [Further version history truncated for brevity]
#  v1.0 2010-03-02
#       Initial release.
#
#  Usage:
#  ./install_dotfiles.sh
#  - Deploys dotfiles and configures user environments.
#  - Automatically detects and sets up configurations for multiple users.
#  - Run with appropriate permissions if modifying system-wide settings.
#
########################################################################

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
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

# Check required commands
check_commands sudo cp mkdir chmod chown id rm ln find zsh

check_sudo

setup_environment() {
    SCRIPTS="$HOME/scripts"
    if [ ! -d "$SCRIPTS" ]; then
        echo "Error: Directory '$SCRIPTS' does not exist. Please create it or specify the correct path." >&2
        exit 1
    fi

    case "$OSTYPE" in
      *darwin*)
        OPTIONS="-v"
        ;;
      *)
        OPTIONS="-vd"
        ;;
    esac
}

mkdir_if_not_exist() {
    while [ "$#" -gt 0 ]
    do
        sudo test -d "$1" || sudo mkdir "$1"
        shift
    done
}

deploy_dotfile() {
    for DOT_FILES in zshrc screenrc vimrc gvimrc gitconfig gitignore condarc gemrc Rprofile emacs
    do
        test -d "$1" && sudo cp "$OPTIONS" "$SCRIPTS/dot_files/dot_$DOT_FILES" "$1/.$DOT_FILES"
    done
    for DOT_FILES in pryrc
    do
        test -d "$1" && test -f "$1/.$DOT_FILES" && sudo rm -vf "$1/.$DOT_FILES"
    done
}

setup_dotemacs() {
    mkdir_if_not_exist \
      "$1/.emacs.d" \
      "$1/.emacs.d/site-lisp" \
      "$1/.emacs.d/anything" \
      "$1/.emacs.d/backups" \
      "$1/.emacs.d/tmp" \
      "$1/.emacs.d/tramp-auto-save" \
      "$1/.emacs.d/auto-save-list"
    sudo chmod 750 "$1/.emacs.d" "$1/.emacs.d/site-lisp" "$1/.emacs.d/anything" "$1/.emacs.d/backups" "$1/.emacs.d/tmp" "$1/.emacs.d/tramp-auto-save" "$1/.emacs.d/auto-save-list"
    sudo touch "$1/.emacs.d/anything/anything-c-adaptive-history"
    test -d /usr/local/etc/emacs.d/elisp && \
      sudo ln -fs /usr/local/etc/emacs.d/elisp "$1/.emacs.d/elisp"
    test -L /usr/local/etc/emacs.d/elisp/elisp && \
      sudo rm -f /usr/local/etc/emacs.d/elisp/elisp
}

mkdir_skelton() {
    mkdir_if_not_exist \
      "$1/.tmp" "$1/.local" "$1/.config" "$1/tmp" "$1/mnt" "$1/local" "$1/var" "$1/etc" "$1/bin" "$1/arc"
    sudo chmod 700 "$1/.tmp" "$1/.local" "$1/.config" "$1/tmp" "$1/mnt" "$1/var"
    sudo chmod 750 "$1/local" "$1/etc" "$1/bin" "$1/arc"
    command -v emacs >/dev/null 2>&1 && setup_dotemacs "$1"
}

deploy_dotfiles() {
    deploy_dotfile "$1"
    mkdir_skelton "$1"
}

deploy_dotfiles_to_others() {
    if [ -d "$1" ]; then
        deploy_dotfiles "$1"
        sudo chown -R "$2:$(id -gn "$2")" "$1"
    fi
}

deploy_dotfiles_to_mac() {
    while [ "$#" -gt 0 ]
    do
        if [ -d "/Users/$1" ]; then
            deploy_dotfiles "/Users/$1"
        fi
        shift
    done
}

deploy_dotfiles_to_linux() {
    while [ "$#" -gt 0 ]
    do
        if [ -d "/home/$1" ]; then
            deploy_dotfiles "/home/$1"
            sudo chown "$1:$(id -gn "$1")" "/home/$1"
            sudo find "/home/$1" -maxdepth 1 -mindepth 1 -exec chown "$1:$(id -gn "$1")" {} +
        fi
        shift
    done
}

bulk_deploy() {
    test -d "/home" && test -d "/home/$USER" && sudo chmod 750 /home/*
    deploy_dotfiles_to_linux \
      debian \
      ubuntu \
      redhat \
      centos \
      sl \
      admin \
      sysadmin \
      sysop \
      ec2-user \
      git \
      automatic \
      fluent \
      mongo \
      plagger \
      twitter \
      tiarra \
      testuser
    deploy_dotfiles_to_mac \
      mac \
      apple \
      adm \
      demo \
      work \
      testuser
    deploy_dotfiles_to_others /var/root root
    deploy_dotfiles_to_others /root root
    deploy_dotfiles_to_others /var/lib/postgresql postgres
    deploy_dotfiles_to_others /var/lib/pgsql postgres
    deploy_dotfiles_to_others /usr/lib/oracle/xe oracle
    deploy_dotfiles_to_others /export/home/solaris solaris
    deploy_dotfiles_to_others /var/lib/jenkins jenkins
}

install_dotfiles() {
    setup_environment "$1"
    bulk_deploy
    test -f "$HOME/.zshrc.zwc" && rm -f "$HOME/.zshrc.zwc"
    cd || exit 1
    zsh -c 'zcompile "$HOME/.zshrc"'

    test -f "$HOME/etc/config.local/dot_gitconfig" && \
      cp "$OPTIONS" "$HOME/etc/config.local/dot_gitconfig" "$HOME/.gitconfig"

    test -f "$HOME/.viminfo" && sudo chown "$USER" "$HOME/.viminfo"
}

install_dotfiles "$@"

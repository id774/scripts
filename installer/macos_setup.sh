#!/bin/sh

########################################################################
# macos_setup.sh: macOS Batch Setup Script
#
#  Description:
#  This script automates the setup of a macOS environment by installing
#  and configuring essential dotfiles (zsh, vim, emacs), Python/IPython
#  settings, sysadmin utility scripts, and cleaning up history files.
#  It also ensures proper ownership and permissions on key directories.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./macos_setup.sh
#
#  Requirements:
#  - Must be run on macOS (Darwin).
#  - SUDO privileges are required.
#  - The $HOME/scripts directory must contain all referenced installer scripts.
#
#  Notes:
#  - This script installs and configures dot_zsh, dot_vim, and dot_emacs if not present.
#  - It installs sysadmin utility scripts and IPython profile.
#  - It resets ownership of key source directories and removes shell/editor histories.
#
#  Exit Conditions:
#  - Exits with error if not run on macOS.
#  - Exits if required commands are missing or not executable.
#  - Exits if sudo privileges are not granted.
#
#  Version History:
#  v1.2 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.1 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.0 2025-03-23
#       Initial release forked from debian_setup.sh for macOS-specific use.
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

# Check if the system is macOS
check_system() {
    if [ "$(uname)" != "Darwin" ]; then
        echo "[ERROR] This script is intended for macOS only." >&2
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

# Verify script environment
setup_environment() {
    SCRIPTS="$HOME/scripts"
    if [ ! -d "$SCRIPTS" ]; then
        echo "[ERROR] Directory '$SCRIPTS' does not exist. Please create it or specify the correct path." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Install various dotfiles configurations
install_dot_files() {
    "$SCRIPTS/installer/install_dotfiles.sh"
}

install_dot_zsh() {
    test -d "$HOME/local/github" || mkdir -p "$HOME/local/github"
    cd "$HOME/local/github" || exit 1

    if [ ! -d "dot_zsh" ]; then
        git clone https://github.com/id774/dot_zsh.git
    else
        cd dot_zsh || exit 1
        if [ -d ".git" ]; then
            git pull
        fi
    fi

    cd "$HOME/local/github/dot_zsh" || exit 1
    ln -snf "$HOME/local/github/dot_zsh" "$HOME/dot_zsh"
    "$HOME/local/github/dot_zsh/install_dotzsh.sh"
}

install_dot_vim() {
    if command -v vim >/dev/null 2>&1; then
        "$SCRIPTS/installer/install_dotvim.sh"
    fi
}

install_dot_emacs() {
    if [ ! -d "$HOME/local/github/dot_emacs" ] && [ ! -d "/usr/local/etc/emacs.d/elisp" ] && command -v emacs >/dev/null 2>&1; then
        test -d "$HOME/local/github" || mkdir -p "$HOME/local/github"
        cd "$HOME/local/github" || exit 1
        git clone https://github.com/id774/dot_emacs.git
        cd || exit 1
        ln -snf "$HOME/local/github/dot_emacs"
        "$HOME/local/github/dot_emacs/install_dotemacs.sh"
    fi
}

# Setup various system utilities
setup_sysadmin_scripts() {
    "$SCRIPTS/installer/setup_sysadmin_scripts.sh" uninstall
    "$SCRIPTS/installer/setup_sysadmin_scripts.sh" install
}

setup_dot_ipython() {
    if [ -x /opt/python/current/bin/python ]; then
        "$SCRIPTS/installer/setup_dot_ipython.sh"
    fi
}

setup_emergencyadmin() {
    "$SCRIPTS/installer/create_emergencyadmin.sh"
}

finder_settings() {
    "$SCRIPTS/installer/macos_finder_settings.sh"
}

folder_localization() {
    "$SCRIPTS/installer/macos_system_folder_localizations.sh" enable
}

fix_compinit() {
    "$SCRIPTS/fix_compinit.sh"
}

# Set permissions for key directories
set_permissions() {
    sudo chown -R root:wheel /opt/python
    sudo chown -R root:wheel /opt/ruby
    sudo chown -R root:wheel /usr/local/src
}

# Erase history files
erase_history() {
    sudo rm -vf "$HOME/.bash_history"
    sudo rm -vf "$HOME/.mysql_history"
    sudo rm -vf "$HOME/.viminfo"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    setup_environment
    check_commands sudo zsh git ln rm chown
    check_sudo
    install_dot_files
    install_dot_zsh
    install_dot_vim
    install_dot_emacs
    setup_sysadmin_scripts
    setup_dot_ipython
    setup_emergencyadmin
    finder_settings
    fix_compinit
    folder_localization
    set_permissions
    erase_history

    echo "[INFO] All macOS setup completed."
    return 0
}

# Execute main function
main "$@"
exit $?

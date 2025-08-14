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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script directly:
#      ./debian_setup.sh
#
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
#  Version History:
#  v1.4 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.3 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.2 2025-03-26
#       Add setup logic for munin-plugins repository and install process_monitoring plugin.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
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
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
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
        ln -snf "$HOME/local/github/dot_emacs" "$HOME/dot_emacs"
        "$HOME/local/github/dot_emacs/install_dotemacs.sh"
    fi
}

# Install cryptographic tools
install_crypt() {
    #if ! command -v des >/dev/null 2>&1; then
    #    "$SCRIPTS/installer/install_des.sh"
    #fi

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

setup_python_symlink() {
    "$SCRIPTS/installer/setup_python_symlink.sh"
}

setup_get_resources() {
    "$SCRIPTS/installer/install_get_resources.sh"
}

setup_chkrootkit() {
    "$SCRIPTS/installer/install_chkrootkit.sh"
}

setup_clamscan() {
    "$SCRIPTS/installer/install_clamscan.sh"
    "$SCRIPTS/installer/disable_freshclam_syslog.sh"
}

setup_munin() {
    test -d "$HOME/local/github" || mkdir -p "$HOME/local/github"
    cd "$HOME/local/github" || exit 1

    if [ ! -d "munin-plugins" ]; then
        git clone https://github.com/id774/munin-plugins.git
    else
        cd munin-plugins || exit 1
        if [ -d ".git" ]; then
            git pull
        fi
    fi
    ln -snf "$HOME/local/github/munin-plugins" "$HOME/munin-plugins"
    "$HOME/local/github/munin-plugins/install_process_monitoring.sh"

    "$SCRIPTS/installer/install_munin.sh"
}

setup_config_files() {
    "$SCRIPTS/installer/setup_iptables.sh"
    "$SCRIPTS/installer/setup_crontab.sh"
    "$SCRIPTS/installer/setup_aliases.sh"
    "$SCRIPTS/installer/setup_pamd.sh"
    "$SCRIPTS/installer/securetty.sh"
    "$SCRIPTS/installer/setup_motd.sh"
    "$SCRIPTS/installer/setup_chkrootkit_opts.sh"
    "$SCRIPTS/installer/setup_memcached_conf.sh"
}

setup_dot_ipython() {
    if [ -x /opt/python/current/bin/python ]; then
        "$SCRIPTS/installer/setup_dot_ipython.sh"
    fi
}

# Set permissions for key directories
set_permissions() {
    sudo chown -R root:root /usr/src
    sudo chown -R root:root /usr/local/src
}

# Configure system settings
configure_sysctl() {
    "$SCRIPTS/installer/configure_sysctl.sh" --apply
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
    check_commands sudo zsh git cut getent ln rm chown
    check_sudo
    set_zsh_to_default
    install_dot_files
    install_dot_zsh
    install_dot_vim
    install_dot_emacs
    install_crypt
    setup_sysadmin_scripts
    setup_python_symlink
    setup_get_resources
    setup_chkrootkit
    setup_clamscan
    setup_munin
    setup_config_files
    setup_dot_ipython
    set_permissions
    configure_sysctl
    erase_history

    echo "[INFO] All Debian setup completed."
    return 0
}

# Execute main function
main "$@"
exit $?

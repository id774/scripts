#!/bin/sh
#
########################################################################
# Batch Installers for Debian
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.5 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v0.4 2012-05-07
#       Correspond to Ubuntu Precise.
#  v0.3 2012-04-16
#       Shape up unnecessary functions.
#  v0.2 2012-01-23
#       Reconstruction CoffeeScript Environment.
#  v0.1 2011-09-28
#       First version.
########################################################################

setup_environment() {
    SCRIPTS="$HOME/scripts"
    if [ ! -d "$SCRIPTS" ]; then
        echo "Error: Directory '$SCRIPTS' does not exist. Please create it or specify the correct path."
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again."
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions."
            exit 126
        fi
    done
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access."
        exit 1
    fi
}

set_zsh_to_default() {
    chsh -s /bin/zsh
    sudo chsh -s /bin/sh root
}

install_dot_vim() {
    sudo locale-gen ja_JP.UTF-8
    $SCRIPTS/installer/install_dotvim.sh
}

install_dot_zsh() {
    test -d $HOME/local/github || mkdir -p $HOME/local/github
    cd $HOME/local/github
    git clone https://github.com/id774/dot_zsh.git
    cd
    ln -s $HOME/local/github/dot_zsh
    $HOME/local/github/dot_zsh/install_dotzsh.sh
}

install_dot_emacs() {
    test -d $HOME/local/github || mkdir -p $HOME/local/github
    cd $HOME/local/github
    git clone https://github.com/id774/dot_emacs.git
    cd
    ln -s $HOME/local/github/dot_emacs
    $HOME/local/github/dot_emacs/install_dotemacs.sh
}

install_dot_files() {
    $SCRIPTS/installer/install_dotfiles.sh
}

install_crypt() {
    $SCRIPTS/installer/install_des.sh
    $SCRIPTS/installer/install_truecrypt.sh
    $SCRIPTS/installer/install_veracrypt.sh
}

configure_sysstat() {
    sudo dpkg-reconfigure sysstat
}

configure_hddtemp() {
    sudo dpkg-reconfigure hddtemp
}

setup_sysadmin_scripts() {
    $SCRIPTS/installer/setup_sysadmin_scripts.sh
}

setup_get_resources() {
    $SCRIPTS/installer/install_get_resources.sh
}

setup_munin() {
    $SCRIPTS/installer/install_munin.sh
}

setup_securetty() {
    $SCRIPTS/securetty.sh
}

configure_sysctl() {
    $SCRIPTS/installer/configure_sysctl.sh --apply
}

permission_for_src() {
    sudo chown -R root:root /usr/src
    sudo chown -R root:root /usr/local/src
}

erase_history() {
    test -f $HOME/.bash_history && sudo rm $HOME/.bash_history
    test -f $HOME/.mysql_history && sudo rm $HOME/.mysql_history
    test -f $HOME/.viminfo && sudo rm $HOME/.viminfo
}

main() {
    setup_environment
    check_commands sudo vi zsh git
    check_sudo
    set_zsh_to_default
    test -d ~/.vim || install_dot_vim
    test -d ~/local/github/dot_zsh || install_dot_zsh
    test -d ~/local/github/dot_emacs || install_dot_emacs
    install_dot_files
    install_crypt
    configure_sysstat
    configure_hddtemp
    setup_sysadmin_scripts
    setup_get_resources
    setup_munin
    setup_securetty
    configure_sysctl
    permission_for_src
    erase_history
}

main "$@"

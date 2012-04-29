#!/bin/sh
#
# This scripts updates environment from 0.6 to 0.7
########################################################################

install_truecrypt() {
    #$SCRIPTS/installer/install_des.sh
    $SCRIPTS/installer/install_truecrypt.sh src
    $SCRIPTS/installer/install_truecrypt.sh win
    $SCRIPTS/installer/install_truecrypt.sh mac
    $SCRIPTS/installer/install_truecrypt.sh $1
}

install_iptables() {
    $SCRIPTS/installer/install_iptables.sh debian
}

deploy_dotfiles() {
    $HOME/local/github/dot_emacs/install_dotemacs.sh
    $HOME/local/github/dot_zsh/install_dotzsh.sh
    $SCRIPTS/installer/install_dotvim.sh
}

install_coffeescript() {
    $SCRIPTS/installer/install_coffeescript.sh
}

install_termtter_plugins() {
    $PRIVATE/installer/install_dottermtter.sh
    $SCRIPTS/installer/install_termtter_plugins.sh
}

setup_web() {
    $HOME/local/github/intraweb-template/install_intraweb.sh
}

setup_sysadmin_scripts() {
    $SCRIPTS/installer/setup_sysadmin_scripts.sh
}

remove_incr_zsh() {
    sudo rm -f /etc/zsh/plugins/incr.zsh*
}

install_dot_files() {
    $SCRIPTS/installer/install_dotfiles.sh
}

operation() {
    export SCRIPTS=$HOME/scripts
    export PRIVATE=$HOME/private/scripts
    install_truecrypt linux-`uname -m`
    install_iptables
    deploy_dotfiles
    install_coffeescript
    install_termtter_plugins
    setup_web
    setup_sysadmin_scripts
    install_dot_files
    #remove_incr_zsh
}

operation $*

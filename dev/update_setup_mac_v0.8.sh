#!/bin/sh
#
# This scripts updates environment from 0.8 to 0.9
########################################################################

install_ruby_and_rails() {
    $SCRIPTS/installer/install_ruby_and_rails.sh
}

install_nodejs_and_coffeescript() {
    $SCRIPTS/installer/install_nodejs.sh
    $SCRIPTS/installer/install_coffeescript.sh
    $SCRIPTS/installer/install_npm.sh
}

install_termtter_plugins() {
    $PRIVATE/installer/install_dottermtter.sh
    $SCRIPTS/installer/install_termtter_plugins.sh
}

deploy_dotfiles() {
    $HOME/local/github/dot_emacs/install_dotemacs.sh
    $HOME/local/github/dot_zsh/install_dotzsh.sh
    $SCRIPTS/installer/install_dotvim.sh
}

remove_incr_zsh() {
    sudo rm -f /etc/zsh/plugins/incr.zsh*
}

install_dot_files() {
    $SCRIPTS/installer/install_dotfiles.sh
}

operation() {
    install_ruby_and_rails
    install_nodejs_and_coffeescript
    install_termtter_plugins
    deploy_dotfiles
    remove_incr_zsh
    install_dot_files
}

operation $*

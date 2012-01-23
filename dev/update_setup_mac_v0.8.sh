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

purge_old_modules() {
    test -d /opt/ruby/1.9.2 && \
      sudo rm -rf /opt/ruby/1.9.2
    test -d /usr/local/src/ruby/ruby-1.9.2-p290 && \
      sudo rm -rf /usr/local/src/ruby/ruby-1.9.2-p290
    test -d /usr/local/src/ruby/ruby-1.9.3-p0 && \
      sudo rm -rf /usr/local/src/ruby/ruby-1.9.3-p0
    test -d /usr/local/src/ruby/branches/ruby_1_9_2 && \
      sudo rm -rf /usr/local/src/ruby/branches/ruby_1_9_2
    test -d /usr/local/src/node.js/node-v0.6.2 && \
      sudo rm -rf /usr/local/src/node.js/node-v0.6.2
    test -d /usr/local/src/node.js/node-v0.6.5 && \
      sudo rm -rf /usr/local/src/node.js/node-v0.6.5
    test -d /usr/local/src/node.js/node-v0.6.6 && \
      sudo rm -rf /usr/local/src/node.js/node-v0.6.6
}

operation() {
    install_ruby_and_rails
    install_nodejs_and_coffeescript
    install_termtter_plugins
    deploy_dotfiles
    remove_incr_zsh
    install_dot_files
    purge_old_modules
}

operation $*

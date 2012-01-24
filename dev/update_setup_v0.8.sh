#!/bin/sh
#
# This scripts updates environment from 0.8 to 0.9
########################################################################

test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts

smart_apt() {
    while [ $# -gt 0 ]
    do
        if [ `aptitude search $1 | awk '/^i/' | wc -l` = 0 ]; then
            sudo apt-get -y install $1
        fi
        shift
    done 
}

setup_apt_source() {
    SOURCESLIST=sources-$DISTRIB_CODENAME.list
    sudo cp $PRIVATE/etc/$SOURCESLIST /etc/apt/sources.list
    sudo vi /etc/apt/sources.list
    sudo apt-get update
}

increase_debian_packages() {
    $SCRIPTS/installer/debian_apt.sh
}

xvfb_packages() {
    smart_apt \
      xvfb \
      fluxbox \
      x11vnc
}

install_ruby_and_rails() {
    $SCRIPTS/installer/install_ruby_and_rails.sh
}

install_nodejs_and_coffeescript() {
    $SCRIPTS/installer/install_nodejs.sh
    $SCRIPTS/installer/install_coffeescript.sh
    $SCRIPTS/installer/install_npm.sh
}

deploy_dotfiles() {
    $HOME/local/github/dot_emacs/install_dotemacs.sh
    $HOME/local/github/dot_zsh/install_dotzsh.sh
    $SCRIPTS/installer/install_dotvim.sh
    $PRIVATE/installer/install_dottermtter.sh
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
    test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
    test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts
    test -f /etc/lsb-release && DISTRIB_CODENAME=lucid
    test -f /etc/lsb-release || DISTRIB_CODENAME=squeeze
    setup_apt_source
    increase_debian_packages
    #xvfb_packages
    install_ruby_and_rails
    install_nodejs_and_coffeescript
    deploy_dotfiles
    remove_incr_zsh
    install_dot_files
    purge_old_modules
}

operation $*

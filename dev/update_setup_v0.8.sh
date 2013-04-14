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
    sudo cp $SCRIPTS/etc/$SOURCESLIST /etc/apt/sources.list
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

install_cronjob() {
    $SCRIPTS/installer/install_get_resources.sh
    $SCRIPTS/installer/install_chkrootkit.sh
    $HOME/deferred-sync/installer/install.sh
}

secure_settings() {
    $SCRIPTS/securetty.sh
}

install_dot_files() {
    $SCRIPTS/installer/install_dotfiles.sh
}

purge_old_modules() {
    test -d /etc/emacs.d
      sudo rm -rf /etc/emacs.d
    test -d /etc/zsh/lib
      sudo rm -rf /etc/zsh/lib
    test -d /etc/zsh/plugins
      sudo rm -rf /etc/zsh/plugins
}

operation() {
    install_ruby_and_rails
    install_nodejs_and_coffeescript
    deploy_dotfiles
    # install_cronjob
    #secure_settings
    install_dot_files
    purge_old_modules
}

operation $*

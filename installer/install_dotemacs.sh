#!/bin/sh

setup_dotemacs() {
    test -d ~/.emacs.d && rm -rf ~/.emacs.d/
    test -f ~/.emacs && rm -f ~/.emacs

    cp $OPTIONS $SCRIPTS/dot_files/dot_emacs $HOME/.emacs
    test -d $TARGET || mkdir -p $TARGET
    cp $OPTIONS $SCRIPTS/dot_files/dot_emacs.d/* $TARGET/
}

setup_rhtml() {
    if [ -d ~/local/github/rhtml ]; then
        cd ~/local/github/rhtml
        git pull
    else
        cd ~/local/github
        git clone git://github.com/eschulte/rhtml.git
        cd ~/local/github/rhtml
    fi
    ln -s ~/local/github/rhtml ~/.emacs.d/elisp/3rd-party
}

setup_rinari() {
    if [ -d ~/local/github/rinari ]; then
        cd ~/local/github/rinari
        git pull
    else
        cd ~/local/github
        git clone git://github.com/eschulte/rinari.git
        cd ~/local/github/rinari
    fi
    git submodule init
    git submodule update
    ln -s ~/local/github/rinari ~/.emacs.d/elisp/3rd-party
}

emacs_private_settings() {
    test -f ~/private/scripts/etc/twitter1-account.el && cp $OPTIONS ~/private/scripts/etc/twitter*-account.el $TARGET/elisp/

    chmod 600 $TARGET/elisp/twitter*-account.el
    vim $TARGET/elisp/proxy.el $TARGET/elisp/emacs-w3m.el $TARGET/elisp/unix-defaults.el
}

test -d $SCRIPTS/dot_files/dot_emacs.d || exit 1
TARGET=$HOME/.emacs.d

case $OSTYPE in
  *darwin*)
    OPTIONS=-Rv
    ;;
  *)
    OPTIONS=-Rvd
    ;;
esac

setup_dotemacs
setup_rhtml
setup_rinari
emacs_private_settings


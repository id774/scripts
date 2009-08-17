#!/bin/sh
#
########################################################################
# Install dot_emacs
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 5/18,2009
#       Stable.
########################################################################

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
    chmod 600 $TARGET/elisp/twitter*-account.el*
    test -f ~/etc/config.local/local.el && cp $OPTIONS ~/etc/config.local/*.el $TARGET/elisp/
    vim $TARGET/elisp/proxy.el $TARGET/elisp/emacs-w3m.el $TARGET/elisp/unix-defaults.el $TARGET/elisp/local.el
}

batch_byte_compile() {
    cd ~/.emacs.d/elisp/3rd-party
    emacs --batch --eval '(byte-compile-file "js2.el")'
    emacs --batch --eval '(byte-compile-file "redo.el")'
    emacs --batch --eval '(byte-compile-file "ruby-block.el")'
    emacs --batch --eval '(byte-compile-file "auto-complete.el")'
    emacs --batch --eval '(byte-compile-file "key-chord.el")'
    emacs --batch --eval '(byte-compile-file "twitter1-mode.el")'
    emacs --batch --eval '(byte-compile-file "twitter2-mode.el")'
    emacs --batch --eval '(byte-compile-file "twitter3-mode.el")'
    emacs --batch --eval '(byte-compile-file "twitter4-mode.el")'
    emacs --batch --eval '(byte-compile-file "twitter5-mode.el")'
    cd ~/.emacs.d/elisp
    emacs --batch --eval '(byte-compile-file "custom.el")'
    emacs --batch --eval '(byte-compile-file "delete-empty-file.el")'
    emacs --batch --eval '(byte-compile-file "emacs-w3m.el")'
    emacs --batch --eval '(byte-compile-file "global-set-key.el")'
    emacs --batch --eval '(byte-compile-file "key-chord-define-global.el")'
    emacs --batch --eval '(byte-compile-file "kill-all-buffers.el")'
    emacs --batch --eval '(byte-compile-file "new-file-p.el")'
    emacs --batch --eval '(byte-compile-file "persistent-scratch.el")'
    emacs --batch --eval '(byte-compile-file "physical-line.el")'
    emacs --batch --eval '(byte-compile-file "proxy.el")'
    emacs --batch --eval '(byte-compile-file "startup.el")'
    emacs --batch --eval '(byte-compile-file "tab4.el")'
    emacs --batch --eval '(byte-compile-file "twitter-key.el")'
    emacs --batch --eval '(byte-compile-file "unix-defaults.el")'
    emacs --batch --eval '(byte-compile-file "utils.el")'
    emacs --batch --eval '(byte-compile-file "view-mode-key.el")'
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
batch_byte_compile


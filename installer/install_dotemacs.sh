#!/bin/sh
#
########################################################################
# Install dot_emacs
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.6 10/4,2010
#       Refactoring.
#  v1.5 8/9,2010
#       Generate account setting files.
#  v1.4 3/9,2010
#       Auto generate twitter elisp.
#  v1.3 3/7,2010
#       Refactoring.
#  v1.2 3/2,2010
#       Byte compile by /usr/bin/emacs on mac default.
#  v1.1 2/21,2010
#       Specify emacs path.
#  v1.0 5/18,2009
#       Stable.
########################################################################

setup_dotemacs() {
    test -d $TARGET && rm -rf $TARGET/
    test -f $HOME/.emacs && rm -f $HOME/.emacs

    cp $OPTIONS $DOT_EMACS/dot_emacs $HOME/.emacs
    test -d $TARGET || mkdir -p $TARGET
    cp $OPTIONS $DOT_EMACS/dot_emacs.d/* $TARGET/
}

gen_twitter_el() {
    cd $TARGET/elisp/3rd-party
    sed s/twitter1/twitter$1/g twitter1-mode.el |\
    sed s/24\)/$2\)/g |\
    sed s/103\)/$3\)/g > twitter$1-mode.el
    cd $TARGET/elisp
    cp twitter1-account.el twitter$1-account.el
}

gen_twitter_els() {
    gen_twitter_el 2 23 102
    gen_twitter_el 3 22 101
    gen_twitter_el 4 21  98
    gen_twitter_el 5 19  96
    gen_twitter_el 6 18  94
}

setup_rhtml() {
    if [ -d $GITHUB/rhtml ]; then
        cd $GITHUB/rhtml
        git pull
    else
        cd $GITHUB
        git clone git://github.com/eschulte/rhtml.git
        cd $GITHUB/rhtml
    fi
}

setup_rinari() {
    if [ -d $GITHUB/rinari ]; then
        cd $GITHUB/rinari
        git pull
    else
        cd $GITHUB
        git clone git://github.com/eschulte/rinari.git
        cd $GITHUB/rinari
    fi
    git submodule init
    git submodule update
}

emacs_private_settings() {
    if [ -f $PRIVATE/etc/twitter1-account.el ]; then
        cp $OPTIONS $PRIVATE/etc/twitter*-account.el $TARGET/elisp/
    fi
    chmod 600 $TARGET/elisp/twitter*-account.el*

    if [ -f $HOME/etc/config.local/local.el ]; then
        cp $OPTIONS $HOME/etc/config.local/*.el $TARGET/elisp/
    fi
    vim $TARGET/elisp/proxy.el $TARGET/elisp/emacs-w3m.el $TARGET/elisp/unix-defaults.el $TARGET/elisp/local.el
}

byte_compile_all() {
    cd ~/.emacs.d/elisp/3rd-party
    $EMACS --batch --eval '(byte-compile-file "js2.el")'
    $EMACS --batch --eval '(byte-compile-file "redo+.el")'
    $EMACS --batch --eval '(byte-compile-file "viewer.el")'
    $EMACS --batch --eval '(byte-compile-file "ruby-block.el")'
    $EMACS --batch --eval '(byte-compile-file "jaspace.el")'
    $EMACS --batch --eval '(byte-compile-file "actionscript-mode.el")'
    $EMACS --batch --eval '(byte-compile-file "auto-complete.el")'
    $EMACS --batch --eval '(byte-compile-file "key-chord.el")'
    $EMACS --batch --eval '(byte-compile-file "anything.el")'
    $EMACS --batch --eval '(byte-compile-file "twitter1-mode.el")'
    $EMACS --batch --eval '(byte-compile-file "twitter2-mode.el")'
    $EMACS --batch --eval '(byte-compile-file "twitter3-mode.el")'
    $EMACS --batch --eval '(byte-compile-file "twitter4-mode.el")'
    $EMACS --batch --eval '(byte-compile-file "twitter5-mode.el")'
    $EMACS --batch --eval '(byte-compile-file "twitter6-mode.el")'
    $EMACS --batch --eval '(byte-compile-file "zlc.el")'
    cd ~/.emacs.d/elisp
    $EMACS --batch --eval '(byte-compile-file "custom.el")'
    $EMACS --batch --eval '(byte-compile-file "delete-empty-file.el")'
    $EMACS --batch --eval '(byte-compile-file "emacs-w3m.el")'
    $EMACS --batch --eval '(byte-compile-file "global-set-key.el")'
    $EMACS --batch --eval '(byte-compile-file "jde-config.el")'
    $EMACS --batch --eval '(byte-compile-file "key-chord-define-global.el")'
    $EMACS --batch --eval '(byte-compile-file "kill-all-buffers.el")'
    $EMACS --batch --eval '(byte-compile-file "minor-mode-hack.el")'
    $EMACS --batch --eval '(byte-compile-file "new-file-p.el")'
    $EMACS --batch --eval '(byte-compile-file "persistent-scratch.el")'
    $EMACS --batch --eval '(byte-compile-file "physical-line.el")'
    $EMACS --batch --eval '(byte-compile-file "proxy.el")'
    $EMACS --batch --eval '(byte-compile-file "startup.el")'
    $EMACS --batch --eval '(byte-compile-file "tab4.el")'
    $EMACS --batch --eval '(byte-compile-file "twitter-key.el")'
    $EMACS --batch --eval '(byte-compile-file "unix-defaults.el")'
    $EMACS --batch --eval '(byte-compile-file "utils.el")'
    $EMACS --batch --eval '(byte-compile-file "view-mode-key.el")'
}

byte_compile_cedet() {
    cd $TARGET/elisp/3rd-party/cedet
    make EMACS=$EMACS
}

byte_compile_jde() {
    $EMACS --batch -Q -f batch-byte-compile $TARGET/elisp/3rd-party/jde/lisp/*.el
}

network_connection() {
    setup_rhtml
    setup_rinari
}

setup_environment() {
    TARGET=$HOME/.emacs.d

    case $OSTYPE in
      *darwin*)
        OPTIONS=-Rv
        test -n "$1" || EMACS=/usr/bin/emacs
        ;;
      *)
        OPTIONS=-Rvd
        test -n "$1" || EMACS=emacs
        ;;
    esac

    test -n "$1" && EMACS=$1
}

install_dotemacs() {
    cd
    setup_environment $*
    setup_dotemacs
    gen_twitter_els
    ping -c 1 -i 3 google.com > /dev/null 2>&1 && network_connection
    ln -s $HOME/local/github/rinari $TARGET/elisp/3rd-party
    ln -s $HOME/local/github/rhtml $TARGET/elisp/3rd-party
    emacs_private_settings
    byte_compile_all
    byte_compile_jde
    #byte_compile_cedet
}

GITHUB=$HOME/local/github
DOT_EMACS=$GITHUB/dot_emacs
test -d $DOT_EMACS || exit 1
install_dotemacs $*

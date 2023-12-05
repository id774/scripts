#!/bin/sh
#
########################################################################
# Install dot_vim
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 2010-03-07
#       Refactoring.
########################################################################

setup_environment() {
    TARGET=$HOME/.vim

    case $OSTYPE in
      *darwin*)
        OPTIONS=-Rv
        ;;
      *)
        OPTIONS=-Rvd
        ;;
    esac
}

install_dotvim() {
    setup_environment
    test -d $TARGET || mkdir -p $TARGET
    cp $OPTIONS $SCRIPTS/dot_files/dot_vim/* $TARGET/
}

test -d $SCRIPTS/dot_files/dot_vim || exit 1
install_dotvim

#!/bin/sh
#
########################################################################
# Install dot_zsh
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 5/20,2010
#       First.
########################################################################

setup_environment() {
    TARGET=$HOME/.zsh/plugins

    case $OSTYPE in
      *darwin*)
        OPTIONS=-Rv
        ;;
      *)
        OPTIONS=-Rvd
        ;;
    esac
}

install_dotzsh() {
    setup_environment
    test -d $TARGET || mkdir -p $TARGET
    cp $OPTIONS $SCRIPTS/dot_files/dot_zsh/* $TARGET/
}

test -d $SCRIPTS/dot_files/dot_zsh || exit 1
install_dotzsh

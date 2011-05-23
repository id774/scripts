#!/bin/sh
#
########################################################################
# Install dot_zsh
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 5/23,2010
#       Install zsh plugins to /etc/zsh/plugins.
#  v0.1 5/20,2010
#       First.
########################################################################

setup_environment() {
    test -n "$1" && export TARGET=$1
    test -n "$1" || export TARGET=/etc/zsh
    #test -n "$1" || export TARGET=$HOME/.zsh
    test -n "$2" || SUDO=sudo
    test -n "$2" && SUDO=

    case $OSTYPE in
      *darwin*)
        OPTIONS=-Rv
        OWNER=root:wheel
        ;;
      *)
        OPTIONS=-Rvd
        OWNER=root:root
        ;;
    esac
}

set_permission() {
    $SUDO chown $OWNER -R $TARGET
}

install_dotzsh() {
    setup_environment $*
    test -d $TARGET || $SUDO mkdir -p $TARGET
    $SUDO cp $OPTIONS $SCRIPTS/dot_files/dot_zsh/* $TARGET/
    test -n "$2" || set_permission
}

test -d $SCRIPTS/dot_files/dot_zsh/plugins || exit 1
install_dotzsh $*

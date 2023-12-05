#!/bin/sh
#
########################################################################
# Install emacs, emacs-w3m and mew
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2012-06-14
#       First.
########################################################################

test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts

install_emacs() {
    VERSION=24.3
    $SCRIPTS/installer/install_emacs.sh $VERSION /opt/emacs/$VERSION
    $SCRIPTS/installer/install_emacs_w3m.sh /opt/emacs/$VERSION
    $SCRIPTS/installer/install_mew.sh /opt/emacs/$VERSION/bin/emacs
}

operation() {
    test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
    test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts
    install_emacs
}

operation $*

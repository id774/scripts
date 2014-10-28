#!/bin/sh
#
########################################################################
# Install Ruby and Rails
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 7/8,2013
#       Cut out purge process.
#  v0.1 7/11,2011
#       First.
########################################################################

install_ruby() {
    $SCRIPTS/installer/install_ruby.sh 214 /opt/ruby/2.1
    $SCRIPTS/installer/install_gems.sh /opt/ruby/2.1
}

operation() {
    test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
    test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts
    $SCRIPTS/installer/purge_obsolete_sources.sh
    install_ruby
}

operation $*

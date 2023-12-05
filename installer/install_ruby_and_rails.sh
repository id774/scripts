#!/bin/sh
#
########################################################################
# Install Ruby and Rails
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 2015-08-06
#       Install mecab and cabocha binding.
#  v1.0 2013-07-08
#       Cut out purge process.
#  v0.1 2011-07-11
#       First.
########################################################################

install_ruby() {
    $SCRIPTS/installer/install_ruby.sh $1 $2
    which mecab > /dev/null && $SCRIPTS/installer/install_mecab_ruby.sh $2
    which cabocha > /dev/null && $SCRIPTS/installer/install_cabocha_ruby.sh $2
    $SCRIPTS/installer/install_gems.sh $2
}

operation() {
    test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
    test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts
    $SCRIPTS/installer/purge_obsolete_sources.sh
    install_ruby 31 /opt/ruby/3.1
}

operation $*

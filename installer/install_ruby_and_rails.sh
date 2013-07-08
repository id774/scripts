#!/bin/sh
#
########################################################################
# Install Ruby and Rails
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 7/8,2013
#       Cut out purge process.
#  v0.9 2/9,2013
#       Update to ruby 2.0.
#  v0.8 2/8,2013
#       Update to ruby 1.9.3-p385.
#  v0.7 1/17,2013
#       Update to ruby 1.9.3-p374.
#  v0.6 1/7,2013
#       Update to ruby 1.9.3-p362.
#  v0.5 11/12,2012
#       Update to ruby 1.9.3-p327.
#  v0.4 10/31,2012
#       Update to ruby 1.9.3-p286.
#  v0.3 2/19,2012
#       Update to ruby 1.9.3-p125.
#  v0.2 1/24,2012
#       Ruby 1.9.3 clean install.
#  v0.1 7/11,2011
#       First.
########################################################################

test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts

install_ruby() {
    $SCRIPTS/installer/install_ruby.sh 200-247 /opt/ruby/2.0
    $SCRIPTS/installer/install_gems.sh /opt/ruby/2.0
}

operation() {
    test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
    test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts
    $SCRIPTS/installer/purge_obsolete_sources.sh
    install_ruby
}

operation $*

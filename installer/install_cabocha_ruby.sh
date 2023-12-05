#!/bin/sh
#
########################################################################
# Install CaboCha Ruby Binding
#  $1 = ruby path
#  $2 = source path
#  $3 = no sudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2014-02-10
#       First.
########################################################################

setup_environment() {
    test -n "$1" && export RUBY=$1/bin/ruby
    test -n "$1" || export RUBY=/opt/ruby/current/bin/ruby
    test -n "$2" && export TARGET=$2
    test -n "$2" || export TARGET=/usr/local/src/cabocha/cabocha-0.67/ruby
    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
    test "$3" = "sudo" && SUDO=sudo
}

source_compile() {
    cd $TARGET
    $SUDO $RUBY extconf.rb
    $SUDO make
    $SUDO make install
}

install_mecab() {
    setup_environment $*
    source_compile $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_mecab $*

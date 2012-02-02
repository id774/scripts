#!/bin/sh
#
########################################################################
# Install gem packages for ruby 1.8
#  $1 = ruby path (ex. /usr/local)
#  $2 $3 $4 = proxy
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2/2,2012
#       First.
########################################################################

install_gem() {
    test -n "$1" && export GEM=$1/bin/gem
    test -n "$1" || export GEM=gem
    export RUBYOPT=rubygems

    sudo $GEM install -v 2.3.14 rails $2 $3 $4
    sudo $GEM install -v 0.4.2 i18n $2 $3 $4
    sudo $GEM install -v 2.8.1 mysql $2 $3 $4
    sudo $GEM install -v 3.0.11 passenger $2 $3 $4
    sudo $GEM install -v 1.1.1 rack $2 $3 $4
    sudo $GEM install -v 0.9.2 rake $2 $3 $4
    sudo $GEM install -v 0.9.7 coderay $2 $3 $4
    sudo $GEM install -v 0.8.2 rubytree $2 $3 $4
    sudo $GEM install sqlite3-ruby $2 $3 $4
    $GEM list --local
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_gem $*

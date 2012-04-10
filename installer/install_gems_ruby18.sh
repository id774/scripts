#!/bin/sh
#
########################################################################
# Install gem packages for ruby 1.8
#  $1 = ruby path (ex. /usr/local)
#  $2 $3 $4 = proxy
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 4/10,2012
#       Update and increase gem packages.
#  v0.1 2/2,2012
#       First.
########################################################################

install_gem() {
    test -n "$1" && export GEM=$1/bin/gem
    test -n "$1" || export GEM=gem
    export RUBYOPT=rubygems

    sudo $GEM install -v 2.3.14 rails $2 $3 $4
    sudo $GEM install -v 0.6.0 i18n $2 $3 $4
    sudo $GEM install -v 2.8.1 mysql $2 $3 $4
    sudo $GEM install -v 3.0.11 passenger $2 $3 $4
    sudo $GEM install -v 1.1.3 bundler $2 $3 $4
    sudo $GEM install -v 1.1.3 rack $2 $3 $4
    sudo $GEM install -v 0.9.2.2 rake $2 $3 $4
    sudo $GEM install -v 3.12 rdoc $2 $3 $4
    sudo $GEM install -v 1.0.6 coderay $2 $3 $4
    sudo $GEM install -v 0.8.2 rubytree $2 $3 $4
    sudo $GEM install -v 1.0.0 daemon_controller $2 $3 $4
    sudo $GEM install -v 0.4.3 edavis10-object_daddy $2 $3 $4
    sudo $GEM install -v 1.5.4 fastercsv $2 $3 $4
    sudo $GEM install -v 1.0.7 fastthread $2 $3 $4
    sudo $GEM install -v 0.10.5 mocha $2 $3 $4
    sudo $GEM install -v 1.6.6 json $2 $3 $4
    sudo $GEM install -v 0.0.1 metaclass $2 $3 $4
    sudo $GEM install -v 0.3.1 net-ldap $2 $3 $4
    sudo $GEM install -v 0.13.2 pg $2 $3 $4
    sudo $GEM install -v 2.13.1 rmagick $2 $3 $4
    sudo $GEM install -v 2.1.8 ruby-openid $2 $3 $4
    sudo $GEM install -v 0.8.2 rubytree $2 $3 $4
    sudo $GEM install -v 3.0.1 shoulda $2 $3 $4
    sudo $GEM install -v 1.0.0 shoulda-context $2 $3 $4
    sudo $GEM install -v 1.0.0 shoulda-matchers $2 $3 $4
    sudo $GEM install -v 1.3.5 sqlite3 $2 $3 $4
    sudo $GEM install -v 1.3.5 sqlite3-ruby $2 $3 $4
    sudo $GEM install -v 0.3.33 tzinfo $2 $3 $4

    sudo $GEM install sqlite3-ruby $2 $3 $4
    $GEM list --local
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_gem $*

#!/bin/sh
#
########################################################################
# Install gem packages
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.6 5/7,2010
#       Update to ruby 1.9.
#  v1.5 5/4,2010
#       Add termtter.
#  v1.4 3/7,2010
#       Refactoring.
#  v1.3 8/27,2009
#       Update to rails 2.3.3.
#  v1.2 1/9,2009
#       Add rspec-rails.
#  v1.1 9/16,2008
#       Add rspec.
#  v1.0 8/15,2008
#       Stable.
########################################################################

install_gem() {
    #export proxy=-r -p $http_proxy
    export proxy=
    test -n "$1" && export GEM=$1
    test -n "$1" || export GEM=gem
    export RUBYOPT=rubygems
    sudo $GEM update $proxy
    sudo $GEM install mongrel $proxy
    sudo $GEM install mongrel_cluster $proxy
    sudo $GEM install mechanize $proxy
    sudo $GEM install Selenium $proxy
    sudo $GEM install vim-ruby $proxy
    sudo $GEM install postgres-pr $proxy
    sudo $GEM install mysql $proxy
    sudo $GEM install BlueCloth $proxy
    sudo $GEM install RedCloth $proxy
    sudo $GEM install net-ssh $proxy
    sudo $GEM install net-sftp $proxy
    sudo $GEM install coverage $proxy
    sudo $GEM install zentest $proxy
    sudo $GEM install capistrano $proxy
    sudo $GEM install magic_multi_connections $proxy
    sudo $GEM install redgreen $proxy
    sudo $GEM install rspec $proxy
    sudo $GEM install rspec-rails $proxy
    sudo $GEM install gherkin $proxy
    sudo $GEM install cucumber $proxy
    sudo $GEM install termtter $proxy
    sudo $GEM install msgpack-rpc $proxy
    sudo $GEM cleanup
    $GEM list --local
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_gem $*

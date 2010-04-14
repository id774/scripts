#!/bin/sh
#
########################################################################
# Install gem packages
#   When use proxy, try following option.
#   -r -p http://proxy.hoge.co.jp:8080
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.4 3/7,2010
#       Refactoring.
#  v1.3 8/27,2009
#       Update to rails 2.3.3
#  v1.2 1/9,2009
#       Add rspec-rails.
#  v1.1 9/16,2008
#       Add rspec.
#  v1.0 8/15,2008
#       Stable.
########################################################################

install_gem() {
    export GEM=/opt/bin/gem
    export RUBYOPT=rubygems
    sudo $GEM update $*
    sudo $GEM install mongrel $*
    sudo $GEM install mongrel_cluster $*
    sudo $GEM install mechanize $*
    sudo $GEM install Selenium $*
    sudo $GEM install vim-ruby $*
    sudo $GEM install postgres-pr $*
    sudo $GEM install mysql $*
    sudo $GEM install BlueCloth $*
    sudo $GEM install RedCloth $*
    sudo $GEM install net-ssh $*
    sudo $GEM install net-sftp $*
    sudo $GEM install coverage $*
    sudo $GEM install zentest $*
    sudo $GEM install capistrano $*
    sudo $GEM install magic_multi_connections $*
    sudo $GEM install redgreen $*
    sudo $GEM install rspec $*
    sudo $GEM install rspec-rails $*
    sudo $GEM install cucumber $*
    sudo $GEM cleanup
    $GEM list --local
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_gem $*

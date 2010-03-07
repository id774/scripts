#!/bin/sh
#
########################################################################
# Install gem packages
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
    export RUBYOPT=rubygems
    sudo gem update
    sudo gem install mongrel
    sudo gem install mongrel_cluster
    sudo gem install mechanize
    sudo gem install Selenium
    sudo gem install vim-ruby
    sudo gem install postgres-pr
    sudo gem install mysql
    sudo gem install BlueCloth
    sudo gem install RedCloth
    sudo gem install net-ssh
    sudo gem install net-sftp
    sudo gem install coverage
    sudo gem install zentest
    sudo gem install capistrano
    sudo gem install magic_multi_connections
    sudo gem install redgreen
    sudo gem install rspec
    sudo gem install rspec-rails
    sudo gem install cucumber
    sudo gem cleanup
    gem list --local
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_gem

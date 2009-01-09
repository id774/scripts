#!/bin/sh
#
########################################################################
# Install gem packages
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 1/9,2009
#       Add rspec-rails.
#  v1.1 9/16,2008
#       Add rspec.
#  v1.0 8/15,2008
#       Stable.
########################################################################

export RUBYOPT=rubygems
sudo gem update
sudo gem install mongrel
sudo gem install mongrel_cluster
sudo gem install mechanize
sudo gem install Selenium
sudo gem install vim-ruby
sudo gem install postgres-pr
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

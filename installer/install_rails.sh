#!/bin/sh
#
########################################################################
# Install Ruby on Rails
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

export RUBYOPT=rubygems
sudo gem update
sudo gem install rails
sudo gem cleanup
gem list --local

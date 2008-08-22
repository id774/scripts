#!/bin/sh
#
########################################################################
# Install RubyGems
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

export RUBYOPT=
mkdir install_rubygems
cd install_rubygems

case "$1" in
  094)
    RUBY_GEMS_ZIP=rubygems-0.9.4
    wget http://rubyforge.org/frs/download.php/20990/$RUBY_GEMS_ZIP.zip
    ;;
  *)
    RUBY_GEMS_ZIP=rubygems-1.1.1
    wget http://rubyforge.org/frs/download.php/35284/$RUBY_GEMS_ZIP.zip
    ;;
esac

unzip $RUBY_GEMS_ZIP
cd $RUBY_GEMS_ZIP
sudo ruby setup.rb
cd ../../
rm -rf install_rubygems
sudo gem update --system
gem list --local

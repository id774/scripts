#!/bin/sh
#
########################################################################
# Install Ruby on Rails
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 8/27,2009
#       Update to 2.3.3.
#  v1.1 1/6,2009
#       Install stand-alone package.
#  v1.0 8/15,2008
#       Stable.
########################################################################

set_rails_permission() {
    case $OSTYPE in
      *darwin*)
        sudo chown -R root:wheel /usr/local/src/rails
        ;;
      *)
        sudo chown -R root:root /usr/local/src/rails
        ;;
    esac
}

extract_rails_zip() {
    test -d $1 && sudo rm -rf $1
    sudo mkdir $1
    cd $1
    sudo unzip ../$1.zip
    sudo rm ../$1.zip
    set_rails_permission
}

wget_rails_zip() {
    sudo wget http://rubyforge.org/frs/download.php/$2/rails-$1.zip
}

select_rails_package() {
    case "$1" in
      116)
        RAILS_VER=1.1.6
        wget_rails_zip $RAILS_VER 12324
        ;;
      126)
        RAILS_VER=1.2.6
        wget_rails_zip $RAILS_VER 28340
        ;;
      202)
        RAILS_VER=2.0.2
        wget_rails_zip $RAILS_VER 29361
        ;;
      205)
        RAILS_VER=2.0.5
        wget_rails_zip $RAILS_VER 45369
        ;;
      210)
        RAILS_VER=2.1.0
        wget_rails_zip $RAILS_VER 37770
        ;;
      212)
        RAILS_VER=2.1.2
        wget_rails_zip $RAILS_VER 45625
        ;;
      222)
        RAILS_VER=2.2.2
        wget_rails_zip $RAILS_VER 47183
        ;;
      *)
        RAILS_VER=2.3.3
        wget_rails_zip $RAILS_VER 60600
        ;;
    esac
    extract_rails_zip rails-$RAILS_VER
    sudo gem install rails -v $RAILS_VER
}

install_rails_standalone() {
    test -d /usr/local/src/rails || sudo mkdir -p /usr/local/src/rails
    cd /usr/local/src/rails
    select_rails_package $1
}

export RUBYOPT=rubygems
install_rails_standalone $1
gem list --local


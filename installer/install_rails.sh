#!/bin/sh
#
########################################################################
# Install Ruby on Rails
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
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

select_rails_package() {
    case "$1" in
      222)
        RAILS_VER=2.2.2
        sudo wget http://rubyforge.org/frs/download.php/47183/rails-$RAILS_VER.zip
        ;;
      212)
        RAILS_VER=2.1.2
        sudo wget http://rubyforge.org/frs/download.php/45625/rails-$RAILS_VER.zip
        ;;
      210)
        RAILS_VER=2.1.0
        sudo wget http://rubyforge.org/frs/download.php/37770/rails-$RAILS_VER.zip
        ;;
      205)
        RAILS_VER=2.0.5
        sudo wget http://rubyforge.org/frs/download.php/45369/rails-$RAILS_VER.zip
        ;;
      202)
        RAILS_VER=2.0.2
        sudo wget http://rubyforge.org/frs/download.php/29361/rails-$RAILS_VER.zip
        ;;
      126)
        RAILS_VER=1.2.6
        sudo wget http://rubyforge.org/frs/download.php/28340/rails-$RAILS_VER.zip
        ;;
      116)
        RAILS_VER=1.1.6
        sudo wget http://rubyforge.org/frs/download.php/12324/rails-$RAILS_VER.zip
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

install_rails_gem() {
    export RUBYOPT=rubygems
    sudo gem update
    sudo gem install rails
    sudo gem cleanup
    gem list --local
}

case "$1" in
  *[0-9]*)
    install_rails_standalone $1
    ;;
  *)
    install_rails_gem
    ;;
esac


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

unzip_rails_zip() {
    test -d $RAILS_ZIP && sudo rm -rf $RAILS_ZIP
    sudo mkdir $RAILS_ZIP
    cd $RAILS_ZIP
    sudo unzip ../$RAILS_ZIP.zip
    sudo rm ../$RAILS_ZIP.zip
    set_rails_permission
}

select_rails_package() {
    case "$1" in
      222)
        RAILS_TARGET=2.2.2
        sudo gem install rails -v $RAILS_TARGET
        RAILS_ZIP=rails-$RAILS_TARGET
        sudo wget http://rubyforge.org/frs/download.php/47183/$RAILS_ZIP.zip
        ;;
      212)
        RAILS_TARGET=2.1.2
        sudo gem install rails -v $RAILS_TARGET
        RAILS_ZIP=rails-$RAILS_TARGET
        sudo wget http://rubyforge.org/frs/download.php/45625/$RAILS_ZIP.zip
        ;;
      210)
        RAILS_TARGET=2.1.0
        sudo gem install rails -v $RAILS_TARGET
        RAILS_ZIP=rails-$RAILS_TARGET
        sudo wget http://rubyforge.org/frs/download.php/37770/$RAILS_ZIP.zip
        ;;
      205)
        RAILS_TARGET=2.0.5
        sudo gem install rails -v $RAILS_TARGET
        RAILS_ZIP=rails-$RAILS_TARGET
        sudo wget http://rubyforge.org/frs/download.php/45369/$RAILS_ZIP.zip
        ;;
      202)
        RAILS_TARGET=2.0.2
        sudo gem install rails -v $RAILS_TARGET
        RAILS_ZIP=rails-$RAILS_TARGET
        sudo wget http://rubyforge.org/frs/download.php/29361/$RAILS_ZIP.zip
        ;;
      126)
        RAILS_TARGET=1.2.6
        sudo gem install rails -v $RAILS_TARGET
        RAILS_ZIP=rails-$RAILS_TARGET
        sudo wget http://rubyforge.org/frs/download.php/28340/$RAILS_ZIP.zip
        ;;
      116)
        RAILS_TARGET=1.1.6
        sudo gem install rails -v $RAILS_TARGET
        RAILS_ZIP=rails-$RAILS_TARGET
        sudo wget http://rubyforge.org/frs/download.php/12324/$RAILS_ZIP.zip
        ;;
    esac
    unzip_rails_zip $RAILS_ZIP
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


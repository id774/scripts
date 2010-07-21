#!/bin/sh
#
########################################################################
# Install Ruby on Rails
#  $1 = rails version (ex. 222)
#  $2 = ruby path (ex. /opt/bin)
#  $3 $4 $5 = proxy
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.6 7/21,2010
#       Fix bug.
#  v1.5 6/30,2010
#       Refactoring.
#  v1.4 5/8,2010
#       Add rails 2.3.5 on ruby 1.9.
#  v1.3 3/7,2010
#       Refactoring.
#  v1.2 8/27,2009
#       Update to 2.3.3.
#  v1.1 1/6,2009
#       Install stand-alone package.
#  v1.0 8/15,2008
#       Stable.
########################################################################

extract_rails_zip() {
    test -d $1 && sudo rm -rf $1
    sudo mkdir $1
    cd $1
    sudo unzip ../$1.zip
    sudo rm ../$1.zip
    sudo chown -R $OWNER /usr/local/src/rails
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
      rails-ruby)
        sudo $GEM install rails sqlite3-ruby rspec-rails \
        cucumber-rails webrat test-unit database_cleaner $3 $4 $5
        exit 0
        ;;
      *)
        RAILS_VER=2.3.3
        wget_rails_zip $RAILS_VER 60600
        ;;
    esac
    extract_rails_zip rails-$RAILS_VER
    sudo $GEM install rails -v $RAILS_VER $3 $4 $5
}

install_rails_standalone() {
    test -d /usr/local/src/rails || sudo mkdir -p /usr/local/src/rails
    cd /usr/local/src/rails
    select_rails_package $*
}

setup_environment() {
    case $OSTYPE in
      *darwin*)
        OWNER=root:wheel
        ;;
      *)
        OWNER=root:root
        ;;
    esac
}

install_rails() {
    setup_environment
    test -n "$2" && export GEM=$2/bin/gem
    test -n "$2" || export GEM=gem
    export RUBYOPT=rubygems
    install_rails_standalone $*
    gem list --local
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_rails $*

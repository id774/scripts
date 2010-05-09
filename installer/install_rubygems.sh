#!/bin/sh
#
########################################################################
# Install RubyGems
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.6 5/7,2010
#       Update to ruby 1.9.
#  v1.5 3/7,2010
#       Refactoring.
#  v1.4 2/25,2010
#       Update to 1.3.6.
#  v1.3 8/27,2009
#       Update to 1.3.5.
#  v1.2 5/21,2009
#       Update to 1.3.3, Keep sources.
#  v1.1 1/1,2009
#       Update to 1.3.1.
#  v1.0 8/15,2008
#       Stable.
########################################################################

setup_environment() {
    case $OSTYPE in
      *darwin*)
        OWNER=root:root
        ;;
      *)
        OWNER=root:wheel
        ;;
    esac
}

install_rubygems() {
    setup_environment
    #export proxy=-r -p $http_proxy
    export proxy=
    test -n "$2" && export GEM=$2
    test -n "$2" || export GEM=gem
    export RUBYOPT=rubygems
    mkdir install_rubygems
    cd install_rubygems

    case "$1" in
      094)
        RUBY_GEMS_ZIP=rubygems-0.9.4
        wget http://rubyforge.org/frs/download.php/20990/$RUBY_GEMS_ZIP.zip
        ;;
      111)
        RUBY_GEMS_ZIP=rubygems-1.1.1
        wget http://rubyforge.org/frs/download.php/35284/$RUBY_GEMS_ZIP.zip
        ;;
      120)
        RUBY_GEMS_ZIP=rubygems-1.2.0
        wget http://rubyforge.org/frs/download.php/38647/$RUBY_GEMS_ZIP.zip
        ;;
      131)
        RUBY_GEMS_ZIP=rubygems-1.3.1
        wget http://rubyforge.org/frs/download.php/45906/$RUBY_GEMS_ZIP.zip
        ;;
      133)
        RUBY_GEMS_ZIP=rubygems-1.3.3
        wget http://rubyforge.org/frs/download.php/56228/$RUBY_GEMS_ZIP.zip
        ;;
      135)
        RUBY_GEMS_ZIP=rubygems-1.3.5
        wget http://rubyforge.org/frs/download.php/60719/$RUBY_GEMS_ZIP.zip
        ;;
      *)
        RUBY_GEMS_ZIP=rubygems-1.3.6
        wget http://rubyforge.org/frs/download.php/69366/$RUBY_GEMS_ZIP.zip
        ;;
    esac

    unzip $RUBY_GEMS_ZIP
    cd $RUBY_GEMS_ZIP
    sudo ruby setup.rb
    cd ..
    sudo mkdir -p /usr/local/src/gems
    sudo cp -av $RUBY_GEMS_ZIP /usr/local/src/gems
    sudo chown -R $OWNER /usr/local/src/gems/$RUBY_GEMS_ZIP
    cd ..
    rm -rf install_rubygems
    sudo $GEM update --system $proxy
    $GEM list --local
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_rubygems $*

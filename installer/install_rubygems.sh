#!/bin/sh
#
########################################################################
# Install RubyGems
#  $1 = gem version (ex. 137)
#  $2 = ruby path (ex. /opt/bin)
#  $3 $4 $5 = proxy
#  $6 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
# v1.11 5/9,2011
#       Update to 1.8.1.
# v1.10 9/16,2010
#       Refactoring.
#  v1.9 7/21,2010
#       Fix bug.
#  v1.8 6/30,2010
#       Refactoring.
#  v1.7 5/16,2010
#       Update to 1.3.7.
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
        OWNER=root:wheel
        ;;
      *)
        OWNER=root:root
        ;;
    esac
}

save_sources() {
    sudo mkdir -p /usr/local/src/gems
    sudo cp -av $RUBY_GEMS_ZIP /usr/local/src/gems
    sudo chown -R $OWNER /usr/local/src/gems/$RUBY_GEMS_ZIP
}

get_rubygems() {
    RUBY_GEMS_ZIP=rubygems-$1
    wget http://rubyforge.org/frs/download.php/$2/$RUBY_GEMS_ZIP.zip
}

install_rubygems() {
    setup_environment
    test -n "$2" && export GEM=$2/bin/gem
    test -n "$2" || export GEM=gem
    test -n "$2" && export RUBY=$2/bin/ruby
    test -n "$2" || export RUBY=ruby
    export RUBYOPT=rubygems
    mkdir install_rubygems
    cd install_rubygems

    case "$1" in
      094)
        get_rubygems 0.9.4 20990
        ;;
      111)
        get_rubygems 1.1.1 35284
        ;;
      120)
        get_rubygems 1.2.0 38647
        ;;
      131)
        get_rubygems 1.3.1 45906
        ;;
      133)
        get_rubygems 1.3.3 56228
        ;;
      134)
        get_rubygems 1.3.4 57644
        ;;
      135)
        get_rubygems 1.3.5 60719
        ;;
      136)
        get_rubygems 1.3.6 69366
        ;;
      137)
        get_rubygems 1.3.7 70697
        ;;
      142)
        get_rubygems 1.4.2 73833
        ;;
      153)
        get_rubygems 1.5.3 74344
        ;;
      162)
        get_rubygems 1.6.2 74446
        ;;
      172)
        get_rubygems 1.7.2 74618
        ;;
      *)
        get_rubygems 1.8.1 74817
        ;;
    esac

    unzip $RUBY_GEMS_ZIP
    cd $RUBY_GEMS_ZIP
    sudo $RUBY setup.rb
    cd ..
    test -n "$6" || save_sources
    cd ..
    rm -rf install_rubygems
    sudo $GEM update --system $3 $4 $5
    $GEM list --local
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_rubygems $*

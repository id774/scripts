#!/bin/sh
#
########################################################################
# Install Ruby
#  $1 = ruby version (ex. 193-448)
#  $2 = ruby path (ex. /opt/bin)
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
# v1.38 5/9,2014
#       Update to ruby 2.1.2, 2.0.0-p485.
# v1.37 5/4,2014
#       Update to ruby 2.0.0-p451, 1.9.3-p545.
# v1.36 2/25,2014
#       Update to ruby 2.1.1.
# v1.35 12/26,2013
#       Update to ruby 2.1.0.
# v1.34 11/26,2013
#       Update to ruby 2.0.0-p353, 1.9.3-p484.
# v1.33 8/11,2013
#       Remove obsolete ruby 1.8.7.
# v1.32 7/5,2013
#       Update to ruby 2.0.0-p247.
# v1.31 6/28,2013
#       Update to ruby 1.9.3-p448, ruby 1.8.7-p374.
# v1.30 5/19,2013
#       Update to ruby 2.0.0-p195, ruby 1.9.3-p429.
# v1.29 4/23,2013
#       Support for ruby 1.8.7-p371.
# v1.28 2/24,2013
#       Add ruby 2.0 stable.
# v1.27 2/9,2013
#       Update to ruby 2.0.
# v1.26 2/8,2013
#       Update to ruby 1.9.3-p385.
# v1.25 1/17,2013
#       Update to ruby 1.9.3-p374.
# v1.24 1/15,2013
#       Add --with-opt-dir option.
# v1.23 1/7,2013
#       Update to ruby 1.9.3-p362.
# v1.22 11/12,2012
#       Update to ruby 1.9.3-p327.
# v1.21 10/31,2012
#       Update to ruby 1.9.3-p286.
# v1.20 4/20,2012
#       Update to ruby 1.9.3-p194.
# v1.19 2/19,2012
#       Update to ruby 1.9.3-p125.
# v1.18 10/31,2011
#       Update to ruby 1.9.3-p0.
# v1.17 7/21,2011
#       Update to ruby 1.9.2-p290, ruby 1.8.7-p352.
# v1.16 2/19,2011
#       Update to ruby 1.9.2-p180, ruby 1.9.1-p431, ruby 1.8.7-p334.
# v1.15 12/27,2010
#       Update to ruby 1.9.2-p136, ruby 1.8.7-p330.
# v1.14 8/17,2010
#       Update to ruby 1.9.1-p430, ruby 1.8.7-p302.
# v1.13 7/14,2010
#       Update to ruby 1.9.1-p429.
# v1.12 6/30,2010
#       Refactoring and fix minor bugs.
# v1.11 6/29,2010
#       Update to ruby 1.8.7-p299.
# v1.10 3/7,2010
#       Refactoring.
#  v1.9 2/20,2010
#       Implement svn up and build.
#  v1.8 2/17,2010
#       Update to ruby 1.8.7-p249, 1.9.1-p378.
#       Add prefix syntax.
#  v1.7 12/26,2009
#       Update to ruby 1.8.7-p248, 1.9.1-p376.
#  v1.6 8/27,2009
#       Update to ruby 1.9.1-p243.
#  v1.5 4/29,2009
#       Put emacs-ruby installer to out.
#  v1.4 1/7,2009
#       Ruby 1.8 svn head.
#  v1.3 12/11,2008
#       Make symbolic link of ruby-mode.el.
#  v1.2 8/24,2008
#       Change directory of ruby trunk source.
#  v1.1 8/12,2008
#       Fix permission.
#  v1.0 6/23,2008
#       Stable.
########################################################################

make_ext_module() {
  while [ $# -gt 0 ]
  do
    cd ext/$1
    sudo $RUBY extconf.rb
    sudo make
    sudo make install
    cd ../..
    shift
  done
}

make_and_install() {
    sudo autoconf
    test -n "$1" || sudo ./configure --with-opt-dir=/usr/local
    test -n "$1" && sudo ./configure --prefix $1 --with-opt-dir=/usr/local
    sudo make
    sudo make install
}

install_trunk() {
    test -d /usr/local/src/ruby || sudo mkdir -p /usr/local/src/ruby
    cd /usr/local/src/ruby
    if [ -d /usr/local/src/ruby/trunk ]; then
        cd trunk
        sudo svn up
    else
        sudo svn co http://svn.ruby-lang.org/repos/ruby/trunk trunk
        cd trunk
    fi
    make_and_install $2
    make_ext_module zlib readline openssl
    sudo chown -R $OWNER /usr/local/src/ruby/trunk
    test -x $SCRIPTS/installer/install_emacs_ruby.sh && \
    $SCRIPTS/installer/install_emacs_ruby.sh /usr/local/src/ruby/trunk/misc
}

install_branch() {
    test -d /usr/local/src/ruby/branches || sudo mkdir -p /usr/local/src/ruby/branches
    cd /usr/local/src/ruby/branches
    if [ -d /usr/local/src/ruby/branches/$1 ]; then
        cd $1
        sudo svn up
    else
        sudo svn co http://svn.ruby-lang.org/repos/ruby/branches/$1/ $1
        cd $1
    fi
    make_and_install $2
    make_ext_module zlib readline openssl
    sudo chown -R $OWNER /usr/local/src/ruby/branches/$1
    test -x $SCRIPTS/installer/install_emacs_ruby.sh && \
    $SCRIPTS/installer/install_emacs_ruby.sh /usr/local/src/ruby/branches/$1/misc
}

install_stable() {
    mkdir install_ruby
    cd install_ruby
    wget ftp://ftp.ruby-lang.org/pub/ruby/$2/ruby-$1.zip
    unzip ruby-$1.zip
    cd ruby-$1
    make_and_install $3
    make_ext_module zlib readline openssl
    cd ..
    test -d /usr/local/src/ruby || sudo mkdir -p /usr/local/src/ruby
    sudo cp $OPTIONS ruby-$1 /usr/local/src/ruby
    sudo chown -R $OWNER /usr/local/src/ruby/ruby-$1
    cd ..
    sudo rm -rf install_ruby
    test -x $SCRIPTS/installer/install_emacs_ruby.sh && \
    $SCRIPTS/installer/install_emacs_ruby.sh /usr/local/src/ruby/ruby-$1/misc
}

setup_environment() {
    test -n "$2" || export RUBY=ruby
    test -n "$2" || test -x /usr/local/bin/ruby && export RUBY=/usr/local/bin/ruby
    test -n "$2" && export RUBY=$2/bin/ruby
    case $OSTYPE in
      *darwin*)
        OPTIONS=-pR
        OWNER=root:wheel
        ;;
      *)
        OPTIONS=-a
        OWNER=root:root
        ;;
    esac
}

install_ruby() {
    setup_environment
    case "$1" in
      212)
        install_stable 2.1.2 2.1 $2
        ;;
      211)
        install_stable 2.1.1 2.1 $2
        ;;
      210-0)
        install_stable 2.1.0 2.1 $2
        ;;
      200-481)
        install_stable 2.0.0-p481 2.0 $2
        ;;
      200-451)
        install_stable 2.0.0-p451 2.0 $2
        ;;
      200-353)
        install_stable 2.0.0-p353 2.0 $2
        ;;
      200-247)
        install_stable 2.0.0-p247 2.0 $2
        ;;
      200-195)
        install_stable 2.0.0-p195 2.0 $2
        ;;
      200-0)
        install_stable 2.0.0-p0 2.0 $2
        ;;
      193-545)
        install_stable 1.9.3-p545 1.9 $2
        ;;
      193-484)
        install_stable 1.9.3-p484 1.9 $2
        ;;
      193-448)
        install_stable 1.9.3-p448 1.9 $2
        ;;
      193-429)
        install_stable 1.9.3-p429 1.9 $2
        ;;
      193-392)
        install_stable 1.9.3-p392 1.9 $2
        ;;
      193-385)
        install_stable 1.9.3-p385 1.9 $2
        ;;
      193-374)
        install_stable 1.9.3-p374 1.9 $2
        ;;
      193-362)
        install_stable 1.9.3-p362 1.9 $2
        ;;
      193-327)
        install_stable 1.9.3-p327 1.9 $2
        ;;
      193-286)
        install_stable 1.9.3-p286 1.9 $2
        ;;
      193-194)
        install_stable 1.9.3-p194 1.9 $2
        ;;
      193-125)
        install_stable 1.9.3-p125 1.9 $2
        ;;
      193-0)
        install_stable 1.9.3-p0 1.9 $2
        ;;
      192-290)
        install_stable 1.9.2-p290 1.9 $2
        ;;
      192-180)
        install_stable 1.9.2-p180 1.9 $2
        ;;
      192-136)
        install_stable 1.9.2-p136 1.9 $2
        ;;
      192-0)
        install_stable 1.9.2-p0 1.9 $2
        ;;
      191-431)
        install_stable 1.9.1-p431 1.9 $2
        ;;
      191-430)
        install_stable 1.9.1-p430 1.9 $2
        ;;
      191-429)
        install_stable 1.9.1-p429 1.9 $2
        ;;
      191-378)
        install_stable 1.9.1-p378 1.9 $2
        ;;
      191-376)
        install_stable 1.9.1-p376 1.9 $2
        ;;
      191-243)
        install_stable 1.9.1-p243 1.9 $2
        ;;
      187-374)
        install_stable 1.8.7-p374 1.8 $2
        ;;
      186-420)
        install_stable 1.8.6-p420 1.8 $2
        ;;
      21-svn)
        install_branch ruby_2_1 $2
        ;;
      20-svn)
        install_branch ruby_2_0_0 $2
        ;;
      18-svn)
        install_branch ruby_1_8 $2
        ;;
      193-svn)
        install_branch ruby_1_9_3 $2
        ;;
      192-svn)
        install_branch ruby_1_9_2 $2
        ;;
      191-svn)
        install_branch ruby_1_9_1 $2
        ;;
      187-svn)
        install_branch ruby_1_8_7 $2
        ;;
      186-svn)
        install_branch ruby_1_8_6 $2
        ;;
      trunk)
        install_trunk $2
        ;;
      *)
        ;;
    esac

    ruby -v
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_ruby $*

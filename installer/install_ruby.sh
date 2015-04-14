#!/bin/sh
#
########################################################################
# Install Ruby
#  $1 = ruby version (ex. 200-481)
#  $2 = ruby path (ex. /opt/bin)
#  $3 = no sudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v2.0 3/11,2015
#       Fix bugs.
#       Specify nosudo option.
#  v1.0 6/23,2008
#       Stable.
########################################################################

make_ext_module() {
  while [ $# -gt 0 ]
  do
    cd ext/$1
    $SUDO $RUBY extconf.rb
    $SUDO make
    $SUDO make install
    cd ../..
    shift
  done
}

make_and_install() {
    $SUDO autoconf
    test -n "$1" || $SUDO ./configure --with-opt-dir=/usr/local
    test -n "$1" && $SUDO ./configure --prefix $1 --with-opt-dir=/usr/local
    $SUDO make
    $SUDO make install
}

install_trunk() {
    test -d /usr/local/src/ruby || $SUDO mkdir -p /usr/local/src/ruby
    cd /usr/local/src/ruby
    if [ -d /usr/local/src/ruby/trunk ]; then
        cd trunk
        $SUDO svn up
    else
        $SUDO svn co http://svn.ruby-lang.org/repos/ruby/trunk trunk
        cd trunk
    fi
    make_and_install $2
    make_ext_module zlib readline openssl
    $SUDO chown -R $OWNER /usr/local/src/ruby/trunk
    test -x $SCRIPTS/installer/install_emacs_ruby.sh && \
    $SCRIPTS/installer/install_emacs_ruby.sh /usr/local/src/ruby/trunk/misc
}

install_branch() {
    test -d /usr/local/src/ruby/branches || $SUDO mkdir -p /usr/local/src/ruby/branches
    cd /usr/local/src/ruby/branches
    if [ -d /usr/local/src/ruby/branches/$1 ]; then
        cd $1
        $SUDO svn up
    else
        $SUDO svn co http://svn.ruby-lang.org/repos/ruby/branches/$1/ $1
        cd $1
    fi
    make_and_install $2
    make_ext_module zlib readline openssl
    $SUDO chown -R $OWNER /usr/local/src/ruby/branches/$1
    test -x $SCRIPTS/installer/install_emacs_ruby.sh && \
    $SCRIPTS/installer/install_emacs_ruby.sh /usr/local/src/ruby/branches/$1/misc
}

install_stable2() {
    mkdir install_ruby
    cd install_ruby
    curl -L http://cache.ruby-lang.org/pub/ruby/$2/ruby-$1.tar.bz2 -O
    tar xjvf ruby-$1.tar.bz2
    cd ruby-$1
    make_and_install $3
    make_ext_module zlib readline openssl
    cd ..
    test -n "$SUDO" && save_sources $*
    cd ..
    $SUDO rm -rf install_ruby
    test -x $SCRIPTS/installer/install_emacs_ruby.sh && \
    $SCRIPTS/installer/install_emacs_ruby.sh /usr/local/src/ruby/ruby-$1/misc
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
    test -n "$SUDO" && save_sources $*
    cd ..
    $SUDO rm -rf install_ruby
    test -x $SCRIPTS/installer/install_emacs_ruby.sh && \
    $SCRIPTS/installer/install_emacs_ruby.sh /usr/local/src/ruby/ruby-$1/misc
}

save_sources() {
    test -d /usr/local/src/ruby || $SUDO mkdir -p /usr/local/src/ruby
    $SUDO cp $OPTIONS ruby-$1 /usr/local/src/ruby
    $SUDO chown -R $OWNER /usr/local/src/ruby/ruby-$1
}

setup_environment() {
    test -n "$2" || export RUBY=ruby
    test -n "$2" || test -x /usr/local/bin/ruby && export RUBY=/usr/local/bin/ruby
    test -n "$2" || test -x /opt/ruby/current/bin/ruby && export RUBY=/opt/ruby/current/bin/ruby
    test -n "$2" && export RUBY=$2/bin/ruby
    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
    test "$3" = "sudo" && SUDO=sudo
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
    setup_environment $*
    case "$1" in
      222)
        install_stable2 2.2.2 2.2 $2
        ;;
      221)
        install_stable2 2.2.1 2.2 $2
        ;;
      220)
        install_stable2 2.2.0 2.2 $2
        ;;
      215)
        install_stable2 2.1.5 2.1 $2
        ;;
      214)
        install_stable 2.1.4 2.1 $2
        ;;
      213)
        install_stable 2.1.3 2.1 $2
        ;;
      212)
        install_stable 2.1.2 2.1 $2
        ;;
      211)
        install_stable 2.1.1 2.1 $2
        ;;
      210-0)
        install_stable 2.1.0 2.1 $2
        ;;
      200-598)
        install_stable 2.0.0-p598 2.0 $2
        ;;
      200-594)
        install_stable 2.0.0-p594 2.0 $2
        ;;
      200-576)
        install_stable 2.0.0-p576 2.0 $2
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
      193-551)
        install_stable 1.9.3-p551 1.9 $2
        ;;
      193-550)
        install_stable 1.9.3-p550 1.9 $2
        ;;
      193-547)
        install_stable 1.9.3-p547 1.9 $2
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
      22-svn)
        install_branch ruby_2_2 $2
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

#!/bin/sh
#
########################################################################
# Install Ruby
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
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

make_and_install() {
    sudo autoconf
    test -n "$1" || sudo ./configure
    test -n "$1" && sudo ./configure --prefix $1
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
    cd ext/zlib
    sudo ruby extconf.rb --with-zlib-include=/usr/include -with-zlib-lib=/usr/lib
    sudo make
    sudo make install
    cd ../openssl
    sudo ruby extconf.rb
    sudo make
    sudo make install
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
    cd ext/zlib
    sudo ruby extconf.rb --with-zlib-include=/usr/include -with-zlib-lib=/usr/lib
    sudo make
    sudo make install
    cd ../openssl
    sudo ruby extconf.rb
    sudo make
    sudo make install
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
    cd ext/zlib
    ruby extconf.rb --with-zlib-include=/usr/include -with-zlib-lib=/usr/lib
    make
    sudo make install
    cd ../openssl
    ruby extconf.rb
    make
    sudo make install
    cd ../../..
    test -d /usr/local/src/ruby || sudo mkdir -p /usr/local/src/ruby
    sudo cp $OPTIONS ruby-$1 /usr/local/src/ruby
    sudo chown -R $OWNER /usr/local/src/ruby/ruby-$1
    cd ..
    sudo rm -rf install_ruby
    test -x $SCRIPTS/installer/install_emacs_ruby.sh && \
    $SCRIPTS/installer/install_emacs_ruby.sh /usr/local/src/ruby/ruby-$1/misc
}

setup_environment() {
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
      191-378)
        install_stable 1.9.1-p378 1.9 $2
        ;;
      191-376)
        install_stable 1.9.1-p376 1.9 $2
        ;;
      191-243)
        install_stable 1.9.1-p243 1.9 $2
        ;;
      187-249)
        install_stable 1.8.7-p249 1.8 $2
        ;;
      187-248)
        install_stable 1.8.7-p248 1.8 $2
        ;;
      187-174)
        install_stable 1.8.7-p174 1.8 $2
        ;;
      187-72)
        install_stable 1.8.7-p72 1.8 $2
        ;;
      186-383)
        install_stable 1.8.6-p383 1.8 $2
        ;;
      186-287)
        install_stable 1.8.6-p287 1.8 $2
        ;;
      18-svn)
        install_branch ruby_1_8 $2
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

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_ruby $*

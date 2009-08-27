#!/bin/sh
#
########################################################################
# Install Ruby
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.6 8/27,2009
#       Update to ruby 1.9.1-p243
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

install_trunk() {
    test -d /usr/local/src/ruby || sudo mkdir -p /usr/local/src/ruby
    cd /usr/local/src/ruby
    sudo svn co http://svn.ruby-lang.org/repos/ruby/trunk trunk
    cd trunk
    sudo autoconf
    sudo ./configure
    sudo make
    sudo make install
    cd ext/zlib
    sudo ruby extconf.rb --with-zlib-include=/usr/include -with-zlib-lib=/usr/lib
    sudo make
    sudo make install
    cd ../openssl
    sudo ruby extconf.rb
    sudo make
    sudo make install
    test -x $SCRIPTS/installer/install_emacs_ruby.sh && $SCRIPTS/installer/install_emacs_ruby.sh /usr/local/src/ruby/trunk/misc
}

install_branch() {
    test -d /usr/local/src/ruby/branches || sudo mkdir -p /usr/local/src/ruby/branches
    cd /usr/local/src/ruby/branches
    sudo svn co http://svn.ruby-lang.org/repos/ruby/branches/$1/ $1
    cd $1
    sudo autoconf
    sudo ./configure
    sudo make
    sudo make install
    cd ext/zlib
    sudo ruby extconf.rb --with-zlib-include=/usr/include -with-zlib-lib=/usr/lib
    sudo make
    sudo make install
    cd ../openssl
    sudo ruby extconf.rb
    sudo make
    sudo make install
    test -x $SCRIPTS/installer/install_emacs_ruby.sh && $SCRIPTS/installer/install_emacs_ruby.sh /usr/local/src/ruby/branches/$1/misc
}

install_stable() {
    mkdir install_ruby
    cd install_ruby
    wget ftp://ftp.ruby-lang.org/pub/ruby/$2/ruby-$1.zip
    unzip ruby-$1.zip
    cd ruby-$1
    ./configure
    make
    sudo make install
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
    cd ..
    sudo rm -rf install_ruby
    test -x $SCRIPTS/installer/install_emacs_ruby.sh && $SCRIPTS/installer/install_emacs_ruby.sh /usr/local/src/ruby/ruby-$1/misc
}

case $OSTYPE in
  *darwin*)
    OPTIONS=-pR
    ;;
  *)
    OPTIONS=-a
    ;;
esac

case "$1" in
  191-243)
    install_stable 1.9.1-p243 1.9
    ;;
  187-174)
    install_stable 1.8.7-p174 1.8
    ;;
  187-72)
    install_stable 1.8.7-p72 1.7
    ;;
  186-383)
    install_stable 1.8.6-p383 1.8
    ;;
  186-287)
    install_stable 1.8.6-p287 1.8
    ;;
  18-svn)
    install_branch ruby_1_8
    ;;
  191-svn)
    install_branch ruby_1_9_1
    ;;
  187-svn)
    install_branch ruby_1_8_7
    ;;
  186-svn)
    install_branch ruby_1_8_6
    ;;
  *)
    install_trunk
    ;;
esac

case $OSTYPE in
  *darwin*)
    sudo chown -R root:wheel /usr/local/src/ruby
    ;;
  *)
    sudo chown -R root:root /usr/local/src/ruby
    ;;
esac

ruby -v

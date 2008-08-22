#!/bin/sh
#
########################################################################
# Install Ruby
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 8/12,2008
#       Fix permission.
#  v1.0 6/23,2008
#       Stable.
########################################################################

install_trunk() {
    test -d /usr/local/src/ruby/branches || sudo mkdir -p /usr/local/src/ruby/branches
    cd /usr/local/src/ruby/branches
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
    sudo cp -a ruby-$1 /usr/local/src/ruby
    cd ..
    rm -rf install_ruby
}

case "$1" in
  187-72)
    install_stable 1.8.7-p72 1.8
    ;;
  186-287)
    install_stable 1.8.6-p287 1.8
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

sudo chown -R root:root /usr/local/src/ruby
ruby -v

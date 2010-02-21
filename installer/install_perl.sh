#!/bin/sh
#
########################################################################
# Install Perl
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 2/20,2010
#       Recactoring.
#  v0.1 6/1,2009
#       First, only for 5.10.0.
########################################################################

make_and_install() {
    cd perl-$1
    test -n "$2" || ./Configure
    test -n "$2" && ./Configure -des -Dprefix=$2
    make
    test -n "$2" || sudo make install
    test -n "$2" && make install
    cd ..
}

get_perl() {
    mkdir install_perl
    cd install_perl
    wget http://www.cpan.org/authors/id/R/RG/RGARCIA/perl-$1.tar.gz
    test -f perl-$1.tar.gz || exit 1
    tar xzvf perl-$1.tar.gz
    test "$2" = "sourceonly" || make_and_install $1 $2
    test -d /usr/local/src/perl || sudo mkdir -p /usr/local/src/perl
    sudo cp $OPTIONS perl-$1 /usr/local/src/perl
    cd ..
    rm -rf install_perl
}

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

test -n "$1" || exit 1
get_perl 5.10.0 $2
sudo chown -R $OWNER /usr/local/src/perl

perl -V

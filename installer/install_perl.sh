#!/bin/sh
#
########################################################################
# Install Perl
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 3/7,2010
#       Refactoring.
#  v0.2 2/20,2010
#       Recactoring.
#  v0.1 6/1,2009
#       First, only for 5.10.0.
########################################################################

setup_environment() {
    VERSION=5.10.0
    test -n "$1" || SUDO=
    test -n "$1" && SUDO=sudo
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

save_sources() {
    test -d /usr/local/src/perl || sudo mkdir -p /usr/local/src/perl
    sudo cp $OPTIONS perl-$VERSION /usr/local/src/perl
    sudo chown -R $OWNER /usr/local/src/perl
}

make_and_install() {
    cd perl-$VERSION
    test -n "$1" || ./Configure
    test -n "$1" && ./Configure -des -Dprefix=$1
    make
    $SUDO make install
    cd ..
}

get_perl() {
    mkdir install_perl
    cd install_perl
    wget http://www.cpan.org/authors/id/R/RG/RGARCIA/perl-$VERSION.tar.gz
    test -f perl-$VERSION.tar.gz || exit 1
    tar xzvf perl-$VERSION.tar.gz
    test "$1" = "sourceonly" || make_and_install $1
    save_sources
    cd ..
    rm -rf install_perl
}

install_perl() {
    setup_environment $*
    test -n "$1" || exit 1
    get_perl $1
    perl -V
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_perl $*

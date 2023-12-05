#!/bin/sh
#
########################################################################
# Install screen
#  $1 = prefix
#  $2 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.4 2017-08-21
#       Fix prefix option bug.
#  v0.3 2014-01-20
#       Renew screen build process.
#  v0.2 2010-09-16
#       Refactoring.
#  v0.1 2010-09-14
#       First.
########################################################################

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

save_sources() {
    test -d /usr/local/src/screen && sudo rm -rf /usr/local/src/screen
    sudo cp $OPTIONS screen /usr/local/src
    sudo chown $OWNER /usr/local/src/screen
    sudo chown -R $OWNER /usr/local/src/screen
}

make_and_install() {
    test -n "$1" || ./configure --prefix=/opt/screen --enable-pam --enable-colors256 --enable-rxvt_osc --enable-use-locale --enable-telnet
    test -n "$1" && ./configure --prefix=$1 --enable-pam --enable-colors256 --enable-rxvt_osc --enable-use-locale --enable-telnet
    sudo make
    sudo make install
}

get_screen() {
    mkdir install_screen
    cd install_screen
    git clone git://git.savannah.gnu.org/screen.git
    cd screen/src
    autoconf
    ./autogen.sh
    test "$1" = "sourceonly" || make_and_install $*
    cd ../..
    test -n "$2" || save_sources
    cd ..
    rm -rf install_screen
}

install_screen() {
    setup_environment $*
    get_screen $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_screen $*

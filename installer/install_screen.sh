#!/bin/sh
#
########################################################################
# Install screen
#  $1 = prefix
#  $2 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 1/20,2014
#       Renew screen build process.
#  v0.2 9/16,2010
#       Refactoring.
#  v0.1 9/14,2010
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
    test -n "$1" && ./configure --prefix=$2 --enable-pam --enable-colors256 --enable-rxvt_osc --enable-use-locale --enable-telnet
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
    test "$1" = "sourceonly" || make_and_install $1 $2
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

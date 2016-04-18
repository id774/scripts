#!/bin/sh
#
########################################################################
# Install emacs
#  $1 = version
#  $2 = prefix
#  $3 = without-x
#  $4 = sudo
#  $5 = sourceonly
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 4/18,2016
#       Renew build options, for Emacs 23.4 on Debian/Ubuntu.
#  v0.4 3/15,2013
#       Update to Emacs 24.3, Change install source path.
#  v0.3 3/17,2011
#       Emacs 23.3.
#  v0.2 9/16,2010
#       Refactoring.
#  v0.1 9/14,2010
#       First.
########################################################################

setup_environment() {
    test -n "$1" && EMACS_VERSION=$1
    test -n "$1" || EMACS_VERSION=23.4
    test -n "$4" || SUDO=sudo
    test -n "$4" && SUDO=
    test "$4" = "sudo" && SUDO=sudo
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
    test -d /usr/local/src/emacs || sudo mkdir -p /usr/local/src/emacs
    sudo cp $OPTIONS emacs-$EMACS_VERSION /usr/local/src/emacs
    sudo chown $OWNER /usr/local/src/emacs
    sudo chown -R $OWNER /usr/local/src/emacs/emacs-$EMACS_VERSION
}

adding_patches() {
    mkdir patches
    cd patches
    wget http://cloud.github.com/downloads/typester/emacs/feature-fullscreen.patch
    wget http://cloud.github.com/downloads/typester/emacs/fix-shiftmodifier-with-ime.patch
    cd ..
    patch -p1 < patches/feature-fullscreen.patch
    patch -p1 < patches/fix-shiftmodifier-with-ime.patch
}

make_and_install() {
    cd emacs-$EMACS_VERSION
    adding_patches $*
    test -n "$3" && test -n "$2" || ./configure --without-x --prefix=$HOME/local/emacs/$EMACS_VERSION
    test -n "$3" || test -n "$2" || ./configure --sharedstatedir=/var/lib --libexecdir=/usr/lib --localstatedir=/var/lib --infodir=/usr/share/info --mandir=/usr/share/man --with-pop=yes --enable-locallisppath=/etc/emacs23:/etc/emacs:/usr/local/share/emacs/$EMACS_VERSION/site-lisp:/usr/local/share/emacs/site-lisp:/usr/share/emacs/$EMACS_VERSION/site-lisp:/usr/share/emacs/site-lisp --with-crt-dir=/usr/lib/i386-linux-gnu --with-x=yes --with-x-toolkit=gtk --with-toolkit-scroll-bars --with-xpm=no --with-gif=no --prefix=$HOME/local/emacs/$EMACS_VERSION
    test -n "$3" && test -n "$2" && ./configure --without-x --prefix=$2
    test -n "$3" || test -n "$2" && ./configure --sharedstatedir=/var/lib --libexecdir=/usr/lib --localstatedir=/var/lib --infodir=/usr/share/info --mandir=/usr/share/man --with-pop=yes --enable-locallisppath=/etc/emacs23:/etc/emacs:/usr/local/share/emacs/$EMACS_VERSION/site-lisp:/usr/local/share/emacs/site-lisp:/usr/share/emacs/$EMACS_VERSION/site-lisp:/usr/share/emacs/site-lisp --with-crt-dir=/usr/lib/i386-linux-gnu --with-x=yes --with-x-toolkit=gtk --with-toolkit-scroll-bars --with-xpm=no --with-gif=no --prefix=$2
    make
    $SUDO make install
    cd ..
}

get_emacs() {
    mkdir install_emacs
    cd install_emacs
    wget http://ftpmirror.gnu.org/emacs/emacs-$EMACS_VERSION.tar.gz
    test -f emacs-$EMACS_VERSION.tar.gz || exit 1
    tar xzvf emacs-$EMACS_VERSION.tar.gz
    test "$5" = "sourceonly" || make_and_install $*
    test -n "$5" || save_sources
    cd ..
    rm -rf install_emacs
}

install_emacs() {
    setup_environment $*
    get_emacs $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_emacs $*

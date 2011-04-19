#!/bin/sh
#
########################################################################
# Install Vim
#  $1 = version
#  $2 = prefix
#  $3 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 9/14,2010
#       Renew to update to vim 7.3.
########################################################################

setup_environment() {
    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
    case $OSTYPE in
      *darwin*)
        OWNER=root:wheel
        ;;
      *)
        OWNER=root:root
        ;;
    esac
}

save_sources() {
    test -d /usr/local/src/vim && sudo rm -rf /usr/local/src/vim
    test -d /usr/local/src/vim || sudo mkdir -p /usr/local/src/vim
    sudo cp vim-7.3.tar.bz2 /usr/local/src/vim
    sudo cp vim-7.3-extra.tar.gz /usr/local/src/vim
    sudo cp vim-7.3-lang.tar.gz /usr/local/src/vim
    sudo cp -a vim73 /usr/local/src/vim
    sudo cp -a patches /usr/local/src/vim
    sudo chown -R $OWNER /usr/local/src/vim
}

build_and_install() {
    cd vim73
    cat ../patches/7.3.* | patch -p0
    test -n "$1" || ./configure --enable-multibyte --enable-xim --enable-fontset --with-features=big --enable-perlinterp --enable-rubyinterp --enable-pythoninterp --prefix=$HOME/local/vim/7.3
    test -n "$1" && (test -n "$2" || ./configure --enable-multibyte --enable-xim --enable-fontset --with-features=big --enable-perlinterp --enable-rubyinterp --enable-pythoninterp --prefix=$1)
    test -n "$1" && test -n "$2" && ./configure --enable-multibyte --enable-xim --enable-fontset --with-features=big --enable-perlinterp --enable-rubyinterp --enable-pythoninterp --prefix=$1 --with-local-dir=$2
    make
    $SUDO make install
}

get_source_and_install() {
    curl -O ftp://ftp.vim.org/pub/vim/unix/vim-7.3.tar.bz2
    curl -O ftp://ftp.vim.org/pub/vim/extra/vim-7.3-extra.tar.gz
    curl -O ftp://ftp.vim.org/pub/vim/extra/vim-7.3-lang.tar.gz
    tar xjvf vim-7.3.tar.bz2
    tar xzvf vim-7.3-extra.tar.gz
    tar xzvf vim-7.3-lang.tar.gz
    mkdir patches
    cd patches
    curl -O 'ftp://ftp.vim.org/pub/vim/patches/7.3/7.3.[001-004]'
    cd ../
    test -n "$3" || save_sources
    build_and_install $1
    cd ../../
}

install_vim() {
    setup_environment $*
    mkdir install_vim
    cd install_vim
    get_source_and_install $*
    rm -rf install_vim
    vim --version
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_vim $*

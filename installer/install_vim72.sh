#!/bin/sh
#
########################################################################
# Install Vim 7.2
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.6 9/14,2010
#       Install to local.
#  v1.5 8/29,2010
#       Update to 7.2.446.
#  v1.4 3/7,2010
#       Refactoring and update to 7.2.385.
#  v1.3 2/20,2010
#       Refactoring.
#  v1.2 3/10,2009
#       Switch wget to curl.
#  v1.1 10/3,2008
#       Implement local build from source.
#  v1.0 8/15,2008
#       Stable.
########################################################################

setup_environment() {
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
    sudo cp vim-7.2.tar.bz2 /usr/local/src/vim
    sudo cp vim-7.2-extra.tar.gz /usr/local/src/vim
    sudo cp vim-7.2-lang.tar.gz /usr/local/src/vim
    sudo cp -a vim72 /usr/local/src/vim
    sudo cp -a patches /usr/local/src/vim
    sudo chown -R $OWNER /usr/local/src/vim
}

build_and_install() {
    cd vim72
    cat ../patches/7.2.* | patch -p0
    test -n "$1" || ./configure --enable-multibyte --enable-xim --enable-fontset --with-features=big --enable-perlinterp --enable-rubyinterp --enable-pythoninterp --prefix=$HOME/local/vim/7.2
    test -n "$1" && (test -n "$2" || ./configure --enable-multibyte --enable-xim --enable-fontset --with-features=big --enable-perlinterp --enable-rubyinterp --enable-pythoninterp --prefix=$1)
    test -n "$1" && test -n "$2" && ./configure --enable-multibyte --enable-xim --enable-fontset --with-features=big --enable-perlinterp --enable-rubyinterp --enable-pythoninterp --prefix=$1 --with-local-dir=$2
    make
    sudo make install
}

get_source_and_install() {
    curl -O ftp://ftp.vim.org/pub/vim/unix/vim-7.2.tar.bz2
    curl -O ftp://ftp.vim.org/pub/vim/extra/vim-7.2-extra.tar.gz
    curl -O ftp://ftp.vim.org/pub/vim/extra/vim-7.2-lang.tar.gz
    tar xjvf vim-7.2.tar.bz2
    tar xzvf vim-7.2-extra.tar.gz
    tar xzvf vim-7.2-lang.tar.gz
    mkdir patches
    cd patches
    curl -O ftp://ftp.vim.org/pub/vim/patches/7.2/7.2.001-100.gz
    gunzip 7.2.001-100.gz
    curl -O ftp://ftp.vim.org/pub/vim/patches/7.2/7.2.101-200.gz
    gunzip 7.2.101-200.gz
    curl -O ftp://ftp.vim.org/pub/vim/patches/7.2/7.2.201-300.gz
    gunzip 7.2.201-300.gz
    curl -O ftp://ftp.vim.org/pub/vim/patches/7.2/7.2.301-400.gz
    gunzip 7.2.301-400.gz
    curl -O 'ftp://ftp.vim.org/pub/vim/patches/7.2/7.2.[401-446]'
    cd ../
    save_sources
    build_and_install $1
    cd ../../
}

install_vim() {
    setup_environment
    mkdir install_vim
    cd install_vim
    get_source_and_install $*
    rm -rf install_vim
    vim --version
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_vim $*

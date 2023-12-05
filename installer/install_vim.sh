#!/bin/sh
#
########################################################################
# Install Vim 7.2
#  $1 = version
#  $2 = prefix
#  $3 = not save to src
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.7 2011-06-23
#       Fix prefix bug.
#  v1.6 2010-09-14
#       Install to local.
#  v1.5 2010-08-29
#       Update to 7.2.446.
#  v1.4 2010-03-07
#       Refactoring and update to 7.2.385.
#  v1.3 2010-02-20
#       Refactoring.
#  v1.2 2009-03-10
#       Switch wget to curl.
#  v1.1 2008-10-03
#       Implement local build from source.
#  v1.0 2008-08-15
#       Stable.
########################################################################

setup_environment() {
    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
    test "$3" = "sudo" && SUDO=sudo
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
    $SUDO make install
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
    test -n "$3" || save_sources
    build_and_install $*
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

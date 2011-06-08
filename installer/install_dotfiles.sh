#!/bin/sh
#
########################################################################
# Install dot_files
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 5/24,2011
#       Add emacs, Refactoring.
#  v1.1 5/3,2011
#       Include .toprc.
#  v1.0 3/2,2010
#       Refactoring.
########################################################################

setup_environment() {
    test -n "$1" && DEFAULT_KEYMAPFILE=$1
    test -n "$1" || DEFAULT_KEYMAPFILE=dot_xmodmaprc_hhklite2

    case $OSTYPE in
      *darwin*)
        OPTIONS=-v
        ;;
      *)
        OPTIONS=-vd
        ;;
    esac
}

deploy_dotfile() {
    for DOT_FILES in zshrc screenrc vimrc gvimrc gitconfig gitignore toprc emacs
    do
        test -d $1 && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES $1/.$DOT_FILES
    done
}

mkdir_skelton() {
    if [ -d $1 ]; then
        sudo test -d $1/.tmp  || sudo mkdir $1/.tmp
        sudo test -d $1/tmp   || sudo mkdir $1/tmp
        sudo test -d $1/mnt   || sudo mkdir $1/mnt
        sudo test -d $1/local || sudo mkdir $1/local
        sudo test -d $1/var   || sudo mkdir $1/var
        sudo test -d $1/etc   || sudo mkdir $1/etc
        sudo test -d $1/bin   || sudo mkdir $1/bin
        sudo test -d $1/arc   || sudo mkdir $1/arc
        sudo chmod 700 $1/.tmp
        sudo chmod 700 $1/tmp
        sudo chmod 700 $1/mnt
        sudo chmod 700 $1/local
        sudo chmod 700 $1/var
        sudo chmod 750 $1/etc
        sudo chmod 750 $1/bin
        sudo chmod 750 $1/arc
        sudo test -d $1/.emacs.d && sudo rm -rf $1/.emacs.d
        sudo mkdir $1/.emacs.d
        sudo test -d $1/.emacs.d/anything || sudo mkdir $1/.emacs.d/anything
        sudo test -d $1/.emacs.d/backups || sudo mkdir $1/.emacs.d/backups
        sudo test -d $1/.emacs.d/tmp || sudo mkdir $1/.emacs.d/tmp
        sudo test -d $1/.emacs.d/tramp-auto-save || sudo mkdir $1/.emacs.d/tramp-auto-save
        sudo test -d $1/.emacs.d/auto-save-list || sudo mkdir $1/.emacs.d/auto-save-list
        sudo chmod 750 $1/.emacs.d
        sudo chmod 750 $1/.emacs.d/anything
        sudo chmod 750 $1/.emacs.d/backups
        sudo chmod 750 $1/.emacs.d/tmp
        sudo chmod 750 $1/.emacs.d/tramp-auto-save
        sudo chmod 750 $1/.emacs.d/auto-save-list
        test -d /etc/emacs.d/elisp && sudo ln -fs /etc/emacs.d/elisp $1/.emacs.d/elisp
    fi
}

deploy_dotfiles() {
    while [ $# -gt 0 ]
    do
        deploy_dotfile $1
        mkdir_skelton $1
        shift
    done 
}

bulk_deploy() {
    deploy_dotfiles \
      /var/root \
      /Users/mac \
      /Users/apple \
      /Users/skrud \
      /Users/demo \
      /Users/work \
      /root \
      /home/debian \
      /home/ubuntu \
      /home/centos \
      /home/sl \
      /home/admin \
      /etc/skel \
      /home/plagger \
      /home/twitter \
      /home/tiarra \
      /home/skrud \
      /home/testuser \
      /var/lib/postgresql \
      /usr/lib/oracle/xe \
      /export/home/solaris
}

install_dotfiles() {
    setup_environment
    bulk_deploy

    test -f ~/.zshrc.zwc && rm -f ~/.zshrc.zwc
    cd
    zsh -c 'zcompile ~/.zshrc'

    if [ -d /etc/xdg/xfce4 ]; then
        sudo cp $SCRIPTS/dot_files/$DEFAULT_KEYMAPFILE /etc/xdg/xfce4/xmodmaprc
        test -f ~/etc/config.local/dot_xmodmaprc && \
          sudo cp $OPTIONS ~/etc/config.local/dot_xmodmaprc \
          /etc/xdg/xfce4/xmodmaprc
        sudo vim /etc/xdg/xfce4/xmodmaprc /etc/xdg/xfce4/xinitrc
    fi

    test -f ~/etc/config.local/dot_gitconfig && \
      cp $OPTIONS ~/etc/config.local/dot_gitconfig ~/.gitconfig
    vim ~/.gitconfig

    test -f ~/.tmp/.gitconfig.bak && rm ~/.tmp/.gitconfig.bak
    test -f ~/.viminfo && sudo chown $USER ~/.viminfo
}

install_dotfiles $*

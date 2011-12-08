#!/bin/sh
#
########################################################################
# Install dot_files
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.5 12/8,2011
#       Corresponding site-lisp.
#  v1.4 10/4,2011
#       Refactoring.
#  v1.3 9/30,2011
#       Fix permission and Refactoring.
#  v1.2 5/24,2011
#       Add emacs, Refactoring.
#  v1.1 5/3,2011
#       Include .toprc.
#  v1.0 3/2,2010
#       Refactoring.
########################################################################

setup_environment() {
    SCRIPTS=$HOME/scripts
    DOT_EMACS=$HOME/dot_emacs
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

setup_dotemacs() {
    sudo test -d $1/.emacs.d || sudo mkdir $1/.emacs.d
    sudo test -d $1/.emacs.d/site-lisp || sudo mkdir $1/.emacs.d/site-lisp
    sudo test -d $1/.emacs.d/anything || sudo mkdir $1/.emacs.d/anything
    sudo test -d $1/.emacs.d/backups || sudo mkdir $1/.emacs.d/backups
    sudo test -d $1/.emacs.d/tmp || sudo mkdir $1/.emacs.d/tmp
    sudo test -d $1/.emacs.d/tramp-auto-save || sudo mkdir $1/.emacs.d/tramp-auto-save
    sudo test -d $1/.emacs.d/auto-save-list || sudo mkdir $1/.emacs.d/auto-save-list
    sudo chmod 750 $1/.emacs.d
    sudo chmod 750 $1/.emacs.d/site-lisp
    test -f $DOT_EMACS/emacs.d/site-lisp/loader.el && \
      sudo cp $DOT_EMACS/emacs.d/site-lisp/loader.el $1/.emacs.d/site-lisp/
    test -f $DOT_EMACS/emacs.d/site-lisp/auto-install.el && \
      sudo cp $DOT_EMACS/emacs.d/site-lisp/auto-install.el $1/.emacs.d/site-lisp/
    sudo emacs --batch -Q -f batch-byte-compile $1/.emacs.d/site-lisp/loader.el
    sudo emacs --batch -Q -f batch-byte-compile $1/.emacs.d/site-lisp/auto-install.el
    sudo chmod 750 $1/.emacs.d/anything
    sudo chmod 750 $1/.emacs.d/backups
    sudo chmod 750 $1/.emacs.d/tmp
    sudo chmod 750 $1/.emacs.d/tramp-auto-save
    sudo chmod 750 $1/.emacs.d/auto-save-list
    sudo touch $1/.emacs.d/anything/anything-c-adaptive-history
    test -d /etc/emacs.d/elisp && sudo ln -fs /etc/emacs.d/elisp $1/.emacs.d/elisp
}

mkdir_skelton() {
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
    which emacs > /dev/null && setup_dotemacs $1
}

deploy_dotfiles() {
    deploy_dotfile $1
    mkdir_skelton $1
}

deploy_dotfiles_to_others() {
    if [ -d $1 ]; then
        deploy_dotfiles $1
        sudo chown -R $2 $1
    fi
}

deploy_dotfiles_to_mac() {
    while [ $# -gt 0 ]
    do
        if [ -d /Users/$1 ]; then
            deploy_dotfiles /Users/$1
            sudo chown -R $1 /Users/$1
        fi
        shift
    done 
}

deploy_dotfiles_to_linux() {
    while [ $# -gt 0 ]
    do
        if [ -d /home/$1 ]; then
            deploy_dotfiles /home/$1
            sudo chown -R $1:$1 /home/$1
        fi
        shift
    done 
}

bulk_deploy() {
    test -d /home && \
      sudo chmod 750 /home/*
    test -d /Users && \
      sudo chmod 700 /Users/*
    deploy_dotfiles_to_linux \
      debian \
      ubuntu \
      redhat \
      centos \
      sl \
      admin \
      plagger \
      twitter \
      tiarra \
      skrud \
      testuser
    deploy_dotfiles_to_mac \
      mac \
      apple \
      skrud \
      demo \
      work \
      testuser
    deploy_dotfiles_to_others /var/root root
    deploy_dotfiles_to_others /root root
    deploy_dotfiles_to_others /var/lib/postgresql postgres
    deploy_dotfiles_to_others /usr/lib/oracle/xe oracle
    deploy_dotfiles_to_others /export/home/solaris solaris
}

install_dotfiles() {
    setup_environment
    bulk_deploy

    test -f $HOME/.zshrc.zwc && rm -f $HOME/.zshrc.zwc
    cd
    zsh -c 'zcompile $HOME/.zshrc'

    if [ -d /etc/xdg/xfce4 ]; then
        sudo cp $SCRIPTS/dot_files/$DEFAULT_KEYMAPFILE /etc/xdg/xfce4/xmodmaprc
        test -f $HOME/etc/config.local/dot_xmodmaprc && \
          sudo cp $OPTIONS $HOME/etc/config.local/dot_xmodmaprc \
          /etc/xdg/xfce4/xmodmaprc
        sudo vi /etc/xdg/xfce4/xmodmaprc /etc/xdg/xfce4/xinitrc
    fi

    test -f $HOME/etc/config.local/dot_gitconfig && \
      cp $OPTIONS $HOME/etc/config.local/dot_gitconfig $HOME/.gitconfig
    vi $HOME/.gitconfig

    test -f $HOME/.tmp/.gitconfig.bak && rm $HOME/.tmp/.gitconfig.bak
    test -f $HOME/.viminfo && sudo chown $USER $HOME/.viminfo
}

install_dotfiles $*

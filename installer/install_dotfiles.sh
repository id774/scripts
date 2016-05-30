#!/bin/sh
#
########################################################################
# Install dot_files
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
# v1.14 1/15,2015
#       Add Rprofile.
# v1.13 10/19,2014
#       Do not change the owner of Users dir on OS X.
# v1.12 5/17,2014
#       Do not change the permissions of Users dir on OS X.
# v1.11 5/22,2013
#       Change permission of local to 750.
# v1.10 4/24,2013
#       Add user ec2-user.
#  v1.9 2/5,2013
#       Add gemrc.
#  v1.8 1/18,2013
#       Delete unnecessary symlinks.
#  v1.7 2/20,2012
#       Change default emacs lisp directory to /usr/local/etc/emacs.d.
#  v1.6 1/11,2012
#       Add line for Jenkins user.
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

mkdir_if_not_exist() {
    while [ $# -gt 0 ]
    do
        sudo test -d $1 || sudo mkdir $1
        shift
    done
}

emacs_batch_byte_compile() {
    while [ $# -gt 0 ]
    do
        sudo emacs --batch -Q -f batch-byte-compile $1
        shift
    done
}

deploy_dotfile() {
    for DOT_FILES in zshrc screenrc vimrc gvimrc gitconfig gitignore toprc gemrc Rprofile emacs
    do
        test -d $1 && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES $1/.$DOT_FILES
    done
}

setup_dotemacs() {
    mkdir_if_not_exist \
      $1/.emacs.d \
      $1/.emacs.d/site-lisp \
      $1/.emacs.d/anything \
      $1/.emacs.d/backups \
      $1/.emacs.d/tmp \
      $1/.emacs.d/tramp-auto-save \
      $1/.emacs.d/auto-save-list
    sudo chmod 750 $1/.emacs.d
    sudo chmod 750 $1/.emacs.d/site-lisp
    test -f $DOT_EMACS/emacs.d/site-lisp/loader.el && \
      sudo cp $DOT_EMACS/emacs.d/site-lisp/loader.el $1/.emacs.d/site-lisp/ && \
      emacs_batch_byte_compile $1/.emacs.d/site-lisp/loader.el
    test -f $DOT_EMACS/emacs.d/site-lisp/auto-install.el && \
      sudo cp $DOT_EMACS/emacs.d/site-lisp/auto-install.el $1/.emacs.d/site-lisp/ && \
      emacs_batch_byte_compile $1/.emacs.d/site-lisp/auto-install.el
    sudo chmod 750 $1/.emacs.d/anything
    sudo chmod 750 $1/.emacs.d/backups
    sudo chmod 750 $1/.emacs.d/tmp
    sudo chmod 750 $1/.emacs.d/tramp-auto-save
    sudo chmod 750 $1/.emacs.d/auto-save-list
    sudo touch $1/.emacs.d/anything/anything-c-adaptive-history
    test -d /usr/local/etc/emacs.d/elisp && \
      sudo ln -fs /usr/local/etc/emacs.d/elisp $1/.emacs.d/elisp
    test -L /usr/local/etc/emacs.d/elisp/elisp && \
      sudo rm /usr/local/etc/emacs.d/elisp/elisp
}

mkdir_skelton() {
    mkdir_if_not_exist \
      $1/.tmp \
      $1/tmp \
      $1/mnt \
      $1/local \
      $1/var \
      $1/etc \
      $1/bin \
      $1/arc
    sudo chmod 700 $1/.tmp
    sudo chmod 700 $1/tmp
    sudo chmod 700 $1/mnt
    sudo chmod 700 $1/var
    sudo chmod 750 $1/etc
    sudo chmod 750 $1/bin
    sudo chmod 750 $1/arc
    sudo chmod 750 $1/local
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
    deploy_dotfiles_to_linux \
      debian \
      ubuntu \
      redhat \
      centos \
      sl \
      admin \
      ec2-user \
      git \
      automatic \
      fluent \
      mongo \
      plagger \
      twitter \
      tiarra \
      testuser
    deploy_dotfiles_to_mac \
      mac \
      apple \
      adm \
      demo \
      work \
      testuser
    deploy_dotfiles_to_others /var/root root
    deploy_dotfiles_to_others /root root
    deploy_dotfiles_to_others /var/lib/postgresql postgres
    deploy_dotfiles_to_others /usr/lib/oracle/xe oracle
    deploy_dotfiles_to_others /export/home/solaris solaris
    deploy_dotfiles_to_others /var/lib/jenkins jenkins
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

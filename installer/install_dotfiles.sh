#!/bin/sh
#
########################################################################
# Install dot_files
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
# v1.19 2025-02-15
#       Remove outdated auto-install.el and unnecessary byte-compilation logic.
# v1.18 2025-02-02
#       Add .local and .config dir.
# v1.17 2022-10-17
#       Remove .toprc.
# v1.16 2016-07-28
#       Include pgsql dir.
# v1.15 2016-07-09
#       Remove obsolete dotfiles.
# v1.14 2015-01-15
#       Add Rprofile.
# v1.13 2014-10-19
#       Do not change the owner of Users dir on OS X.
# v1.12 2014-05-17
#       Do not change the permissions of Users dir on OS X.
# v1.11 2013-05-22
#       Change permission of local to 750.
# v1.10 2013-04-24
#       Add user ec2-user.
#  v1.9 2013-02-05
#       Add gemrc.
#  v1.8 2013-01-18
#       Delete unnecessary symlinks.
#  v1.7 2012-02-20
#       Change default emacs lisp directory to /usr/local/etc/emacs.d.
#  v1.6 2012-01-11
#       Add line for Jenkins user.
#  v1.5 2011-12-08
#       Corresponding site-lisp.
#  v1.4 2011-10-04
#       Refactoring.
#  v1.3 2011-09-30
#       Fix permission and Refactoring.
#  v1.2 2011-05-24
#       Add emacs, Refactoring.
#  v1.1 2011-05-03
#       Include .toprc.
#  v1.0 2010-03-02
#       Refactoring.
########################################################################

setup_environment() {
    SCRIPTS=$HOME/scripts
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

deploy_dotfile() {
    for DOT_FILES in zshrc screenrc vimrc gvimrc gitconfig gitignore condarc gemrc Rprofile emacs
    do
        test -d $1 && sudo cp $OPTIONS $SCRIPTS/dot_files/dot_$DOT_FILES $1/.$DOT_FILES
    done
    for DOT_FILES in pryrc
    do
        test -d $1 && test -f $1/.$DOT_FILES && sudo rm -vf $1/.$DOT_FILES
    done
}

setup_dotemacs() {
    sudo rm -rf $1/.emacs.d/site-lisp
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
    sudo chmod 750 $1/.emacs.d/anything
    sudo chmod 750 $1/.emacs.d/backups
    sudo chmod 750 $1/.emacs.d/tmp
    sudo chmod 750 $1/.emacs.d/tramp-auto-save
    sudo chmod 750 $1/.emacs.d/auto-save-list
    sudo touch $1/.emacs.d/anything/anything-c-adaptive-history
    test -d /usr/local/etc/emacs.d/elisp && \
      sudo ln -fs /usr/local/etc/emacs.d/elisp $1/.emacs.d/elisp
    test -L /usr/local/etc/emacs.d/elisp/elisp && \
      sudo rm -f /usr/local/etc/emacs.d/elisp/elisp
}

mkdir_skelton() {
    mkdir_if_not_exist \
      $1/.tmp \
      $1/.local \
      $1/.config \
      $1/tmp \
      $1/mnt \
      $1/local \
      $1/var \
      $1/etc \
      $1/bin \
      $1/arc
    sudo chmod 700 $1/.tmp
    sudo chmod 700 $1/.local
    sudo chmod 700 $1/.config
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
      sysadmin \
      sysop \
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
    deploy_dotfiles_to_others /var/lib/pgsql postgres
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

    test -f $HOME/.tmp/.gitconfig.bak && rm -f $HOME/.tmp/.gitconfig.bak
    test -f $HOME/.viminfo && sudo chown $USER $HOME/.viminfo
}

install_dotfiles $*

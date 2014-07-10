#!/bin/sh
#
########################################################################
# Batch Installers for Debian
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.4 5/7,2012
#       Correspond to Ubuntu Precise.
#  v0.3 4/16,2012
#       Shape up unnecessary functions.
#  v0.2 1/23,2012
#       Reconstruction CoffeeScript Environment.
#  v0.1 9/28,2011
#       First version.
########################################################################

set_zsh_to_default() {
    chsh -s /bin/zsh
    sudo chsh -s /bin/sh root
}

fix_apt_listbugs() {
    sudo vi /etc/apt/apt.conf.d/10apt-listbugs*
}

configure_exim4() {
    sudo dpkg-reconfigure exim4-config
}

install_dot_vim() {
    $SCRIPTS/installer/install_dotvim.sh
}

install_dot_zsh() {
    test -d $HOME/local/github || mkdir -p $HOME/local/github
    cd $HOME/local/github
    git clone git://github.com/id774/dot_zsh.git
    cd
    ln -s $HOME/local/github/dot_zsh
    $HOME/local/github/dot_zsh/install_dotzsh.sh
}

install_dot_emacs() {
    test -d $HOME/local/github || mkdir -p $HOME/local/github
    cd $HOME/local/github
    git clone git://github.com/id774/dot_emacs.git
    cd
    ln -s $HOME/local/github/dot_emacs
    $HOME/local/github/dot_emacs/install_dotemacs.sh
}

install_dot_files() {
    $SCRIPTS/installer/install_dotfiles.sh
}

install_truecrypt() {
    $SCRIPTS/installer/install_des.sh
    $SCRIPTS/installer/install_truecrypt.sh src
    #$SCRIPTS/installer/install_truecrypt.sh win
    #$SCRIPTS/installer/install_truecrypt.sh mac
    $SCRIPTS/installer/install_truecrypt.sh $1
}

sshfs_modules() {
    sudo vi /etc/modules
}

configure_sysstat() {
    sudo dpkg-reconfigure sysstat
    sudo vi /etc/default/sysstat
}

configure_hddtemp() {
    sudo dpkg-reconfigure hddtemp
}

configure_smartmontools() {
    sudo vi /etc/default/smartmontools
}

setup_sysadmin_scripts() {
    $SCRIPTS/installer/setup_sysadmin_scripts.sh
}

setup_web() {
    test -d $HOME/local/github || mkdir -p $HOME/local/github
    cd $HOME/local/github
    git clone git://github.com/id774/intraweb-template.git
    cd
    ln -s $HOME/local/github/intraweb-template
    $HOME/local/github/intraweb-template/install_intraweb.sh
}

setup_rc_local() {
    $SCRIPTS/installer/install_rclocal.sh
}

setup_ntp() {
    sudo vi /etc/ntp.conf
}

setup_get_resources() {
    $SCRIPTS/installer/install_get_resources.sh
}

setup_munin() {
    $SCRIPTS/installer/install_munin.sh
}

setup_fail2ban() {
    sudo vi /etc/fail2ban/jail.conf
}

change_default() {
    sudo vi /etc/profile
    sudo vi /etc/crontab
    sudo vi /etc/anacrontab
    sudo vi /etc/pam.d/su
    sudo vi /etc/ssh/sshd_config
    sudo vi /etc/pam.d/sshd
    sudo vi /etc/pam.d/login
    sudo vi /etc/motd
    sudo vi /etc/fstab
    sudo vi /etc/hosts
    $SCRIPTS/securetty.sh
}

customize_for_ubuntu() {
    sudo vi /etc/resolvconf/resolv.conf.d/base /etc/resolv.conf
    sudo ln -fs /var/log/syslog /var/log/messages
}

setup_grub() {
    test -f /etc/grub.conf && \
      sudo vi /etc/grub.conf
    test -f /boot/grub/menu.lst && \
      sudo vi /boot/grub/menu.lst
    test -f /etc/default/grub && \
      sudo vi /etc/default/grub && sudo update-grub2
}

setup_group_and_passwd() {
    sudo vi /etc/group
    sudo vi /etc/passwd
    sudo passwd root
    sudo vi /etc/sudoers $SCRIPTS/etc/sudoers
}

permission_for_src() {
    sudo chown -R root:root /usr/src
    sudo chown -R root:root /usr/local/src
}

erase_history() {
    test -f $HOME/.bash_history && sudo rm $HOME/.bash_history
    test -f $HOME/.mysql_history && sudo rm $HOME/.mysql_history
    test -f $HOME/.viminfo && sudo rm $HOME/.viminfo
}

operation() {
    set_zsh_to_default
    fix_apt_listbugs
    configure_exim4
    install_dot_vim
    install_dot_zsh
    install_dot_emacs
    install_dotfiles
    install_truecrypt linux-`uname -m`
    sshfs_modules
    configure_sysstat
    configure_hddtemp
    configure_smartmontools
    setup_sysadmin_scripts
    #setup_web
    setup_rc_local
    setup_ntp
    setup_get_resources
    setup_munin
    setup_fail2ban
    change_default
    test -f /etc/lsb-release && customize_for_ubuntu
    setup_grub
    setup_group_and_passwd
    permission_for_src
    erase_history
}

operation $*

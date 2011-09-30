#!/bin/sh
#
########################################################################
# Batch Custom Installers for Debian
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 9/28,2011
#       First version.
########################################################################

install_pppconfig() {
    $SCRIPTS/installer/install_pppconfig.sh
}

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

install_paco() {
    $SCRIPTS/installer/install_paco.sh
}

build_emacs() {
    $SCRIPTS/installer/install_emacs.sh 23.3 /opt/emacs/23.3
    $SCRIPTS/installer/install_emacs_w3m.sh 23.3 /opt/emacs/23.3
    sudo ln -fs /opt/emacs/23.3/bin/emacs /opt/bin/emacs
}

install_navi2ch() {
    $SCRIPTS/installer/install_navi2ch.sh
}

build_vim() {
    $SCRIPTS/installer/install_ncurses.sh
    $SCRIPTS/installer/install_vim.sh
}

install_iptables() {
    $SCRIPTS/installer/install_iptables.sh debian
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

install_mew() {
    $SCRIPTS/installer/install_mew.sh /opt/emacs/23.3/bin/emacs
}

install_dot_files() {
    $SCRIPTS/installer/install_dotfiles.sh
}

install_truecrypt() {
    #$SCRIPTS/installer/install_des.sh
    #$SCRIPTS/installer/install_crypt.sh src
    #$SCRIPTS/installer/install_crypt.sh win
    #$SCRIPTS/installer/install_crypt.sh mac
    $SCRIPTS/installer/install_crypt.sh $1
}

install_sqlite() {
    $SCRIPTS/installer/install_sqlite.sh
}

install_svn() {
    $SCRIPTS/installer/install_debian_svn.sh
}

sshfs_modules() {
    sudo vi /etc/modules
}

configure_samba() {
    sudo update-rc.d -f samba remove
    sudo cp $SCRIPTS/etc/smb.conf /etc/samba/smb.conf
    sudo smbpasswd -a $USER
}

install_postgres() {
    $SCRIPTS/installer/install_postgres.py install
}

install_mysql() {
    $SCRIPTS/installer/install_mysql.py install -c
}

install_kvm() {
    if [ `egrep '^flags.*(vmx|svm)' /proc/cpuinfo | wc -l` != 0 ]; then
        sudo addgroup $USER libvirtd
        sudo addgroup $USER kvm
    fi
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

install_ruby_and_rails() {
    $SCRIPTS/installer/install_ruby_and_rails.sh
}

install_coffeescript() {
    $SCRIPTS/installer/install_coffeescript.sh
}

install_python() {
    $SCRIPTS/installer/install_python.sh $1 /opt/python/$1
    $SCRIPTS/config/update-alternatives-python.sh
}

install_python_framework() {
    $SCRIPTS/installer/install_python_framework.sh
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

add_blacklist() {
    test -r /etc/modprobe.d/blacklist && \
      sudo vi /etc/modprobe.d/blacklist
    test -r /etc/modprobe.d/blacklist.conf && \
      sudo vi /etc/modprobe.d/blacklist.conf
}

install_apache2ssl() {
    $SCRIPTS/installer/install_apache2ssl.sh
}

install_termtter_plugins() {
    cd $HOME/local/github
    git clone git://github.com/id774/termtter-plugins.git
    cd
    ln -s $HOME/local/github/termtter-plugins
    $PRIVATE/installer/install_dottermtter.sh
    $SCRIPTS/installer/install_termtter_plugins.sh
}

install_plagger_plugins() {
    $SCRIPTS/installer/install_plagger_plugins.sh
}

setup_ntp() {
    sudo vi /etc/ntp.conf
}

install_munin() {
    $SCRIPTS/installer/install_munin.sh
}

get_share_documents() {
    wget http://big.freett.com/railsinstall2/share-documents.tar.gz
    sudo tar xzvf share-documents.tar.gz -C /usr/local/share
    rm share-documents.tar.gz
    sudo chmod -R 755 /usr/local/share/share-documents
    ln -s /usr/local/share/share-documents $HOME/share
}

change_default() {
    sudo vi /etc/profile
    sudo vi /etc/crontab
    sudo vi /etc/anacrontab
    sudo vi /etc/pam.d/su
    sudo vi /etc/ssh/sshd_config
    sudo vi /etc/pam.d/sshd
    sudo vi /etc/pam.d/login
    sudo vi /etc/fstab
    sudo vi /etc/hosts
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
    install_pppconfig
    set_zsh_to_default
    fix_apt_listbugs
    configure_exim4
    #install_paco
    #build_emacs
    #install_navi2ch
    #build_vim
    install_iptables
    install_dot_vim
    install_dot_zsh
    install_dot_emacs
    #install_mew
    install_dotfiles
    install_truecrypt linux-amd64
    #install_sqlite
    #install_svn
    sshfs_modules
    #configure_samba
    #install_postgres
    #install_mysql
    #install_kvm
    configure_sysstat
    configure_hddtemp
    configure_smartmontools
    #install_ruby_and_rails
    #install_coffeescript
    #install_python
    #install_python_framework
    setup_sysadmin_scripts
    #setup_web
    setup_rc_local
    add_blacklist
    install_apache2ssl
    #install_termtter_plugins
    #install_plagger_plugins
    setup_ntp
    #install_munin
    #get_share_documents
    change_default
    setup_grub
    setup_group_and_passwd
    permission_for_src
    erase_history
}

operation

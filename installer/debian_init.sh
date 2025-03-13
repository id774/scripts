#!/bin/sh
#
########################################################################
# Initial Setup Script for Ubuntu/Debian
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v4.0 2011-09-28
#       Reconstruction.
#  v3.2 2011-09-09
#       Remove truecrypt version number.
#  v3.1 2011-07-11
#       Packed off ruby and frameworks.
#  v3.0 2011-06-16
#       Fork from Initial Setup Script, cut off debian apt.
#  v2.5 2011-05-24
#       For zsh framework.
#  v2.4 2011-03-28
#       Remove building ruby 1.9 from default.
#  v2.3 2011-02-28
#       Switch aptitude to apt-get.
#  v2.2 2010-08-30
#       Add KVM.
#  v2.1 2010-08-07
#       Update to truecrypt 7.
#  v2.0 2010-07-21
#       Correspond to Lucid.
# v1.27 2010-06-29
#       Update to ruby 1.8.7-p299.
# v1.26 2010-06-07
#       Add sysvconfig.
# v1.25 2010-05-07
#       Update to ruby 1.9 as default.
# v1.24 2010-03-03
#       Add Debian Developer Tools.
# v1.23 2010-02-17
#       Update to ruby 1.8.7-p249, 1.9.1-p378.
#       Add python install.
# v1.22 2010-02-02
#       Add sysklogd.
# v1.21 2009-12-26
#       Update to ruby 1.8.7-p248, 1.9.1-p376.
# v1.20 2009-12-17
#       Add xfwm4-themes.
# v1.19 2009-10-05
#       Add exiftool.
# v1.18 2009-08-27
#       Update to rails 2.3.3.
# v1.17 2009-07-31
#       Remove opera.
# v1.16 2009-05-18
#       Remove uim-el, and update misc setup.
# v1.15 2009-05-13
#       Add ubuntu-ja, OpenOffice.org, codec, icons.
#       Setting reserved blocks percentage of ext3 filesystem to 1%.
# v1.14 2009-04-29
#       Add GNU GLOBAL.
# v1.13 2009-03-11
#       Disable local build vim.
# v1.12 2009-01-19
#       Add monitoring tools.
# v1.11 2009-01-18
#       Update to truecrypt 6.1a.
# v1.10 2008-12-31
#       Add emacs-w3m.
#  v1.9 2008-12-26
#       Add paco.
#  v1.8 2008-12-11
#       Add Bitstream Vera Sans Mono, set emacs default, purge vim-gnome.
#  v1.7 2008-11-06
#       Change root shell from zsh to bash.
#  v1.6 2008-10-30
#       Emacs snapshot as default.
#  v1.5 2008-10-24
#       Add sysstat.
#  v1.4 2008-10-14
#       Splash various problems of initial setup and set debian as default.
#  v1.3 2008-09-25
#       Auto tune2fs when using LVM.
#  v1.2 2008-09-23
#       Add curl.
#  v1.1 2008-08-14
#       Automatic Install for gpg,browser,iptable,trac,passenger,
#       and Install Xfce4.
#  v1.0 2008-05-12
#       Stable, for Ubuntu Hardy.
#  v0.6 2008-04-17
#       Remove samba, adding sshfs.
#  v0.5 2008-01-28
#       Last setup added.
#  v0.4 2007-11-23
#       PostgreSQL added.
#  v0.3 2007-10-16
#       Save source code.
#  v0.2 2007-09-04
#       Add Optional Application Install.
#  v0.1 2007-08-27
#       First version.
########################################################################

test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts

operation() {
    # Environment
    $SCRIPTS/installer/debian_env.sh

    # Packages
    $SCRIPTS/installer/debian_apt.sh

    # Customize
    $SCRIPTS/installer/debian_batch_installers.sh

    # Desktop Packages
    #$SCRIPTS/installer/debian_desktop_apt.sh

    # Desktop Customize
    #$SCRIPTS/installer/debian_desktop_installers.sh
}

# debian?
test -f /etc/debian_version || exit 1

operation $*

#!/bin/sh
#
########################################################################
# Initial Setup Script for Ubuntu/Debian
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v4.0 9/28,2011
#       Reconstruction.
#  v3.2 9/9,2011
#       Remove truecrypt version number.
#  v3.1 7/11,2011
#       Packed off ruby and frameworks.
#  v3.0 6/16,2011
#       Fork from Initial Setup Script, cut off debian apt.
#  v2.5 5/24,2011
#       For zsh framework.
#  v2.4 3/28,2011
#       Remove building ruby 1.9 from default.
#  v2.3 2/28,2011
#       Switch aptitude to apt-get.
#  v2.2 8/30,2010
#       Add KVM.
#  v2.1 8/7,2010
#       Update to truecrypt 7.
#  v2.0 7/21,2010
#       Correspond to Lucid.
# v1.27 6/29,2010
#       Update to ruby 1.8.7-p299.
# v1.26 6/7,2010
#       Add sysvconfig.
# v1.25 5/7,2010
#       Update to ruby 1.9 as default.
# v1.24 3/3,2010
#       Add Debian Developer Tools.
# v1.23 2/17,2010
#       Update to ruby 1.8.7-p249, 1.9.1-p378.
#       Add python install.
# v1.22 2/2,2010
#       Add sysklogd.
# v1.21 12/26,2009
#       Update to ruby 1.8.7-p248, 1.9.1-p376.
# v1.20 12/17,2009
#       Add xfwm4-themes.
# v1.19 10/5,2009
#       Add exiftool.
# v1.18 8/27,2009
#       Update to rails 2.3.3.
# v1.17 7/31,2009
#       Remove opera.
# v1.16 5/18,2009
#       Remove uim-el, and update misc setup.
# v1.15 5/13,2009
#       Add ubuntu-ja, OpenOffice.org, codec, icons.
#       Setting reserved blocks percentage of ext3 filesystem to 1%.
# v1.14 4/29,2009
#       Add GNU GLOBAL.
# v1.13 3/11,2009
#       Disable local build vim.
# v1.12 1/19,2009
#       Add monitoring tools.
# v1.11 1/18,2009
#       Update to truecrypt 6.1a.
# v1.10 12/31,2008
#       Add emacs-w3m.
#  v1.9 12/26,2008
#       Add paco.
#  v1.8 12/11,2008
#       Add Bitstream Vera Sans Mono, set emacs default, purge vim-gnome.
#  v1.7 11/06,2008
#       Change root shell from zsh to bash.
#  v1.6 10/30,2008
#       Emacs snapshot as default.
#  v1.5 10/24,2008
#       Add sysstat.
#  v1.4 10/14,2008
#       Splash various problems of initial setup and set debian as default.
#  v1.3 9/25,2008
#       Auto tune2fs when using LVM.
#  v1.2 9/23,2008
#       Add curl.
#  v1.1 8/14,2008
#       Automatic Install for gpg,browser,iptable,trac,passenger,
#       and Install Xfce4.
#  v1.0 5/12,2008
#       Stable, for Ubuntu Hardy.
#  v0.6 4/17,2008
#       Remove samba, adding sshfs.
#  v0.5 1/28,2008
#       Last setup added.
#  v0.4 11/23,2007
#       PostgreSQL added.
#  v0.3 10/16,2007
#       Save source code.
#  v0.2 9/4,2007
#       Add Optional Application Install.
#  v0.1 8/27,2007
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

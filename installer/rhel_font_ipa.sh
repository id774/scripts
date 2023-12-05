#!/bin/sh
#
########################################################################
# Install IPA font for RHEL
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 2011-09-29
#       Refactoring.
#  v1.0 2008-08-16
#       Stable.
########################################################################

install_ipa_font() {
    VERSION=5.0.3_i686
    wget http://www.grass-japan.org/FOSS4G/ipafonts/grass$VERSION-pc-linux-i18n-ipafull-gnu_bin.tar.gz
    wget http://www.grass-japan.org/FOSS4G/ipafonts/grass5_i686-pc-linux-i18n-ipafull-gnu_install.sh
    chmod 700 grass5_i686-pc-linux-i18n-ipafull-gnu_install.sh
    sudo grass5_i686-pc-linux-i18n-ipafull-gnu_install.sh \
      grass$VERSION-pc-linux-i18n-ipafull-gnu_bin.tar.gz $HOME/tmp/grass5 $HOME/tmp/bin
    sudo cp $HOME/tmp/grass5/fonts/*.ttf \
      /usr/share/fonts/japanese/TrueType/
    chmod 700 $HOME/tmp/bin/grass5uninstall.sh
    sudo $HOME/tmp/bin/grass5uninstall.sh
    sudo cp $SCRIPTS/etc/local.conf /etc/fonts/local.conf
    cd /usr/share/fonts/japanese/TrueType/
    sudo mv sazanami-mincho.ttf sazanami-mincho.ttf.old
    sudo mv sazanami-gothic.ttf sazanami-gothic.ttf.old
    sudo ln -s ipam.ttf sazanami-mincho.ttf
    sudo ln -s ipag.ttf sazanami-gothic.ttf
    sudo fc-cache -fv .
    sudo rm -rf $HOME/tmp/grass5
    sudo rm -rf $HOME/tmp/bin
}

install_ipa_font

#!/bin/sh
#
########################################################################
# Install IPA font for CentOS 5
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/16,2008
#       Stable.
########################################################################

wget http://www.grass-japan.org/FOSS4G/ipafonts/grass5.0.3_i686-pc-linux-i18n-ipafull-gnu_bin.tar.gz
wget http://www.grass-japan.org/FOSS4G/ipafonts/grass5_i686-pc-linux-i18n-ipafull-gnu_install.sh
chmod 700 grass5_i686-pc-linux-i18n-ipafull-gnu_install.sh
sudo grass5_i686-pc-linux-i18n-ipafull-gnu_install.sh grass5.0.3_i686-pc-linux-i18n-ipafull-gnu_bin.tar.gz ~/tmp/grass5 ~/tmp/bin
sudo cp ~/tmp/grass5/fonts/*.ttf /usr/share/fonts/japanese/TrueType/
chmod 700 ~/tmp/bin/grass5uninstall.sh
sudo ~/tmp/bin/grass5uninstall.sh
sudo cp $SCRIPTS/etc/local.conf /etc/fonts/local.conf
cd /usr/share/fonts/japanese/TrueType/
sudo mv sazanami-mincho.ttf sazanami-mincho.ttf.old
sudo mv sazanami-gothic.ttf sazanami-gothic.ttf.old
sudo ln -s ipam.ttf sazanami-mincho.ttf
sudo ln -s ipag.ttf sazanami-gothic.ttf
sudo fc-cache -fv .
sudo rm -rf ~/tmp/grass5
sudo rm -rf ~/tmp/bin

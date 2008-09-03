#!/bin/sh
#
########################################################################
# Install IPAMonaFonts for Debian GNU/Linux
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 9/3,2008
#       Stable.
########################################################################

mkdir install_ipamonafonts
cd install_ipamonafonts

IPAMonaFontsVersion=1.0.8
wget http://www.geocities.jp/ipa_mona/opfc-ModuleHP-1.1.1_withIPAMonaFonts-$IPAMonaFontsVersion.tar.gz
tar xzvf opfc-ModuleHP-1.1.1_withIPAMonaFonts-$IPAMonaFontsVersion.tar.gz
cd opfc-ModuleHP-1.1.1_withIPAMonaFonts-$IPAMonaFontsVersion/fonts
test -d $HOME/.fonts || mkdir -p $HOME/.fonts
cp -v *.ttf $HOME/.fonts/
cp $SCRIPTS/dot_files/dot_fonts.conf $HOME/.fonts.conf
fc-cache -fv

cd ../../../
rm -rf install_ipamonafonts

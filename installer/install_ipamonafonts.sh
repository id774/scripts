#!/bin/sh
#
########################################################################
# Install IPAMonaFonts for Debian GNU/Linux
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 2010-03-07
#       Refactoring.
#  v1.0 2008-09-03
#       Stable.
########################################################################

setup_environment() {
    test -n "$1" || IPAMonaFontsVersion=1.0.8
    test -n "$1" && IPAMonaFontsVersion=$1
}

install_ipamonafonts() {
    setup_environment $*
    mkdir install_ipamonafonts
    cd install_ipamonafonts
    wget http://www.geocities.jp/ipa_mona/opfc-ModuleHP-1.1.1_withIPAMonaFonts-$IPAMonaFontsVersion.tar.gz
    tar xzvf opfc-ModuleHP-1.1.1_withIPAMonaFonts-$IPAMonaFontsVersion.tar.gz
    cd opfc-ModuleHP-1.1.1_withIPAMonaFonts-$IPAMonaFontsVersion/fonts
    test -d $HOME/.fonts || mkdir -p $HOME/.fonts
    cp -v *.ttf $HOME/.fonts/
    cp $SCRIPTS/dot_files/dot_fonts.conf $HOME/.fonts.conf
    fc-cache -fv

    cd ../../../
    rm -rf install_ipamonafonts
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_ipamonafonts $*

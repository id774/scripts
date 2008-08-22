#!/bin/sh
#
########################################################################
# Install VimColor
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

mkdir install_vimcolor
cd install_vimcolor
wget http://page.freett.com/railsinstall2/vimcolor.zip
unzip vimcolor.zip

case $OSTYPE in
  *darwin*)
    OPTIONS=-v
    ;;
  *)
    OPTIONS=-av
    ;;
esac

TARGET=/usr/share/vim/vim70/colors
if [ -d $TARGET ]; then
    echo "Copying to $TARGET"
    sudo cp $OPTIONS * $TARGET
    sudo chown -R root:root $TARGET
fi

TARGET=/usr/share/vim/vim71/colors
if [ -d $TARGET ]; then
    echo "Copying to $TARGET"
    sudo cp $OPTIONS * $TARGET
    sudo chown -R root:root $TARGET
fi

TARGET=/usr/local/share/vim/vim71/colors
if [ -d $TARGET ]; then
    echo "Copying to $TARGET"
    sudo cp $OPTIONS * $TARGET
    sudo chown -R root:root $TARGET
fi

TARGET=$HOME/.vim/colors
if [ -d $TARGET ]; then
    echo "Copying to $TARGET"
    cp $OPTIONS * $TARGET
fi

cd ..
rm -rf install_vimcolor

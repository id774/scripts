#!/bin/sh
#
########################################################################
# Install VimScripts
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

mkdir install_vimscripts
cd install_vimscripts
wget http://page.freett.com/railsinstall2/vimscripts.tar.gz
tar xzvf vimscripts.tar.gz

case $OSTYPE in
  *darwin*)
    OPTIONS=-v
    ;;
  *)
    OPTIONS=-av
    ;;
esac

TARGET=/usr/share/vim/vim70
if [ -d $TARGET ]; then
    echo "Copying to $TARGET"
    sudo cp $OPTIONS vimscripts/* $TARGET
    sudo chown -R root:root $TARGET
fi

TARGET=/usr/share/vim/vim71
if [ -d $TARGET ]; then
    echo "Copying to $TARGET"
    sudo cp $OPTIONS vimscripts/* $TARGET
    sudo chown -R root:root $TARGET
fi

TARGET=/usr/local/share/vim/vim71
if [ -d $TARGET ]; then
    echo "Copying to $TARGET"
    sudo cp $OPTIONS vimscripts/* $TARGET
    sudo chown -R root:root $TARGET
fi

cd ..
rm -rf install_vimscripts

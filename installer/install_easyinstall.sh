#!/bin/sh
#
########################################################################
# Install EasyInstall
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 1/29,2009
#       First version.
########################################################################

TARGET_PATH=`python -c "from distutils.sysconfig import get_python_lib; print get_python_lib()"`

wget http://peak.telecommunity.com/dist/ez_setup.py
sudo python ez_setup.py -U setuptools
sudo cp ez_setup.py $TARGET_PATH/ez_setup.py
rm ez_setup.py


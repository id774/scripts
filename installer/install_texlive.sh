#!/bin/sh
#
########################################################################
# Install texlive (for Debian 7.0/Ubuntu 14.04 or later)
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 6/2,2014
#       If Ubuntu 12.04 LTS, Add texlive backports repo.
#  v0.1 5/21,2014
#       First.
########################################################################

# If Ubuntu 12.04 LTS, Add texlive backports repo.
if [ -f /etc/issue ]; then
    ISSUE=`cat /etc/issue | head -n 1`
    DIST=`echo $ISSUE | awk '{print $1}'`
    if [[ $DIST =~ "Ubuntu" ]]; then
        VER=`echo $ISSUE | awk '{print $2}'`
        if [[ "$VER" =~ ^12\.04 ]]; then
            echo "It seems that platfrom is Ubuntu 12.04 LTS"
            echo "Adding texlive backports repository"
            sudo apt-add-repository ppa:texlive-backports/ppa
            sudo apt-get -y update
        fi
    fi
fi

sudo apt-get -y install texlive
sudo apt-get -y install texlive-lang-cjk
sudo apt-get -y install xdvik-ja
sudo apt-get -y install dvipsk-ja
sudo apt-get -y install gv
sudo apt-get -y install texlive-fonts-recommended texlive-fonts-extra


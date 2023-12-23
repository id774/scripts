#!/bin/sh
#
########################################################################
# Install texlive (for Debian 7.0/Ubuntu 14.04 or later)
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 2023-12-23
#       Refactored for POSIX compliance. Replaced Bash-specific syntax
#       with POSIX standard commands and structures. Enhanced portability
#       and compatibility across different UNIX-like systems.
#  v0.2 2014-06-02
#       If Ubuntu 12.04 LTS, Add texlive backports repo.
#  v0.1 2014-05-21
#       First.
########################################################################

#!/bin/sh

# If Ubuntu 12.04 LTS, Add texlive backports repo.
if [ -f /etc/issue ]; then
    ISSUE=$(cat /etc/issue | head -n 1)
    DIST=$(echo "$ISSUE" | awk '{print $1}')
    case $DIST in
        Ubuntu)
            VER=$(echo "$ISSUE" | awk '{print $2}')
            case $VER in
                12.04*)
                    echo "It seems that platform is Ubuntu 12.04 LTS"
                    echo "Adding texlive backports repository"
                    sudo apt-add-repository ppa:texlive-backports/ppa
                    sudo apt-get -y update
                    ;;
            esac
            ;;
    esac
fi

sudo apt-get -y install texlive
sudo apt-get -y install texlive-lang-cjk
sudo apt-get -y install xdvik-ja
sudo apt-get -y install dvipsk-ja
sudo apt-get -y install gv
sudo apt-get -y install texlive-fonts-recommended texlive-fonts-extra


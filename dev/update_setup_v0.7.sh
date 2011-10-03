#!/bin/sh
#
# This scripts updates environment from 0.7 to 0.8
########################################################################

increase_debian_packages
    if [ `aptitude search digitools | awk '/^i/' | wc -l` = 0 ]; then
        sudo apt-get -y install digitools
    fi

operation() {
    export SCRIPTS=$HOME/scripts
    export PRIVATE=$HOME/private/scripts
    increase_debian_packages
}

operation $*

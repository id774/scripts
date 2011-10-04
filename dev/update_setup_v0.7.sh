#!/bin/sh
#
# This scripts updates environment from 0.7 to 0.8
########################################################################

increase_debian_packages() {
    $SCRIPTS/installer/debian_apt.sh
}

operation() {
    export SCRIPTS=$HOME/scripts
    export PRIVATE=$HOME/private/scripts
    increase_debian_packages
}

operation $*

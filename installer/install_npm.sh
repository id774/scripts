#!/bin/sh
#
########################################################################
# Install npm modules
#  $1 = npm path (ex. /opt)
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 1/23,2012
#       First.
########################################################################

install_npm() {
    test -n "$1" && export NPM=$1/bin/npm
    test -n "$1" || export NPM=npm
    sudo $NPM install js2coffee -g
    sudo $NPM install mysql -g
    sudo $NPM install express -g
    sudo $NPM install ejs -g
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_npm $*

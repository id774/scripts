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
    sudo $NPM install coffee-script -g
    sudo $NPM install coffeelint -g
    sudo $NPM install jsonlint -g
    sudo $NPM install mysql -g
    sudo $NPM install express -g
    sudo $NPM install jade -g
    sudo $NPM install spark -g
    sudo $NPM install ejs -g
    sudo $NPM install less -g
    sudo $NPM install uglify-js -g
    sudo $NPM install request -g
    sudo $NPM install commander -g
    sudo $NPM install sequelize -g
    sudo $NPM install sqlite3 -g
    sudo $NPM install mongoose -g
    sudo $NPM install aws-sdk -g
    #sudo $NPM install hiredis redis -g
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_npm $*

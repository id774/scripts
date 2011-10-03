#!/bin/sh
#
# This scripts updates environment from 0.7 to 0.8
########################################################################

smart_apt() {
    while [ $# -gt 0 ]
    do
        if [ `aptitude search $1 | awk '/^i/' | wc -l` = 0 ]; then
            sudo apt-get -y install $1
        fi
        shift
    done 
}


increase_debian_packages() {
    smart_apt digitools
    smart_apt ctags global
    smart_apt libxslt1-dev libxslt-ruby python-libxslt1
    smart_apt dpkg-dev lintian debhelper yada equivs cvs-buildpackage \
      dupload fakeroot devscripts debget
    smart_apt apt-listchanges apt-listbugs
    smart_apt nkf clisp
    smart_apt libcurl4-gnutls-dev
    smart_apt openjdk-6-jdk
    smart_apt mew stunnel ca-certificates
}

operation() {
    export SCRIPTS=$HOME/scripts
    export PRIVATE=$HOME/private/scripts
    increase_debian_packages
}

operation $*

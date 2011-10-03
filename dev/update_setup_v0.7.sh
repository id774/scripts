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
    smart_apt \
      digitools \
      ctags global \
      libxslt1-dev libxslt-ruby python-libxslt1 \
      dpkg-dev lintian debhelper yada equivs cvs-buildpackage \
      dupload fakeroot devscripts debget \
      apt-listchanges apt-listbugs \
      nkf clisp \
      libcurl4-gnutls-dev \
      openjdk-6-jdk \
      mew stunnel ca-certificates
}

operation() {
    export SCRIPTS=$HOME/scripts
    export PRIVATE=$HOME/private/scripts
    increase_debian_packages
}

operation $*

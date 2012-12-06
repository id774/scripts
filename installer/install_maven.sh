#!/bin/sh
#
########################################################################
# Install Maven
#  $1 = version
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 12/6,2012
#       First.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=3.0.4
    test -n "$1" && VERSION=$1
    case $OSTYPE in
      *darwin*)
        OWNER=root:wheel
        ;;
      *)
        OWNER=root:root
        ;;
    esac
}

install_maven() {
    setup_environment $*

    mkdir install_maven
    cd install_maven

    wget http://ftp.jaist.ac.jp/pub/apache/maven/maven-3/$VERSION/binaries/apache-maven-$VERSION-bin.tar.gz
    tar xzvf apache-maven-$VERSION-bin.tar.gz
    test -d /opt/maven/$VERSION && sudo rm -rf /opt/maven/$VERSION
    sudo mkdir -p /opt/maven/$VERSION
    sudo mv apache-maven-$VERSION/* /opt/maven/$VERSION
    sudo chown -R $OWNER /opt/maven/$VERSION

    cd ..
    rm -rf install_maven
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_maven $*

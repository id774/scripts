#!/bin/sh
#
########################################################################
# Install Cassandra
#  $1 = version
#  $2 = not mkdir lib and log
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2013-06-13
#       First.
########################################################################

setup_environment() {
    test -n "$1" || VERSION=1.2.5
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

mkdir_lib_and_log() {
    test -d /var/lib/cassandra || sudo mkdir /var/lib/cassandra
    sudo chown -R $OWNER /var/lib/cassandra
    test -d /var/log/cassandra || sudo mkdir /var/log/cassandra
    sudo chown -R $OWNER /var/log/cassandra
}

install_cassandra() {
    setup_environment $*

    mkdir install_cassandra
    cd install_cassandra

    wget http://ftp.riken.jp/net/apache/cassandra/$VERSION/apache-cassandra-$VERSION-bin.tar.gz
    tar xzvf apache-cassandra-$VERSION-bin.tar.gz
    mv apache-cassandra-$VERSION $VERSION

    test -d /opt/cassandra || sudo mkdir -p /opt/cassandra
    test -d /opt/cassandra/$VERSION && sudo rm -rf /opt/cassandra/$VERSION
    sudo mv $VERSION /opt/cassandra/
    sudo chown -R $OWNER /opt/cassandra/$VERSION
    cd ..
    rm -rf install_cassandra

    cd /opt/cassandra
    test -L current || sudo ln -s $VERSION current
    sudo chown -h $OWNER current

    test -n "$2" || mkdir_lib_and_log
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_cassandra $*

#!/bin/sh
#
########################################################################
# Install MongoDB
#  $1 = version
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 10/27,2014
#       Setup debian services.
#  v0.1 7/8,2013
#       First.
########################################################################

setup_environment() {
    test -n "$1" && VERSION=$1
    test -n "$1" || VERSION=2.4.8
    SYSTEM_ID=linux-`uname -m`
    case $OSTYPE in
      *darwin*)
        OWNER=root:wheel
        ;;
      *)
        OWNER=root:root
        ;;
    esac
}

mkdir_if_not_exist(){
    test -d $1 || sudo mkdir -p $1
}

rmdir_if_exist(){
    test -d $1 && sudo rm -rf $1
}

rm_if_exist_symlink(){
    test -L $1 && sudo rm -f $1
}

set_permission() {
    sudo chown -R $1:$1 /data/db
    sudo chown -R $1:$1 /opt/mongo/$VERSION
    sudo chmod -R g+w,o-rwx /data/db/
}

create_user() {
    sudo useradd -m $1
    sudo chsh -s /bin/zsh $1
}

create_symlink() {
    cd /opt/mongo
    rm_if_exist_symlink current
    sudo ln -fs $VERSION current
}

setup_debian_service() {
    sudo cp ~/scripts/etc/init.d/mongod.conf /etc/opt/mongod.conf
    sudo chown root:root /etc/opt/mongod.conf
    sudo cp ~/scripts/etc/init.d/mongod /etc/init.d/mongod
    sudo chown root:root /etc/init.d/mongod
    sudo update-rc.d mongod defaults
    sudo /etc/init.d/mongod start
}

deploy_mongodb() {
    mkdir_if_not_exist /data/db
    mkdir_if_not_exist /opt/mongo
    rmdir_if_exist /opt/mongo/$VERSION
    sudo mv $VERSION /opt/mongo/
    create_user mongo
    set_permission mongo
}

install_mongodb() {
    mkdir install_mongodb
    cd install_mongodb
    case "$SYSTEM_ID" in
      linux-i386 | linux-i686)
        curl http://downloads.mongodb.org/linux/mongodb-linux-i686-$VERSION.tgz > mongodb.tgz
        tar xzvf mongodb.tgz
        mv mongodb-linux-i686-$VERSION $VERSION
        deploy_mongodb
        ;;
      linux-amd64 | linux-x86_64)
        curl http://downloads.mongodb.org/linux/mongodb-linux-x86_64-$VERSION.tgz > mongodb.tgz
        tar xzvf mongodb.tgz
        mv mongodb-linux-x86_64-$VERSION $VERSION
        deploy_mongodb
        ;;
    esac
    cd ..
    rm -rf install_mongodb
}

install_main() {
    setup_environment $*
    install_mongodb $*
    test -f /etc/debian_version && setup_debian_service
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_main $*

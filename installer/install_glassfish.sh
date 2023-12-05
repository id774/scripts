#!/bin/sh
#
########################################################################
# Install glassfish
#
#  $1 = version
#  $2 = path
#  $3 = no sudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 2014-07-03
#       Using update-rc.d, fix some bugs.
#  v0.1 2014-06-29
#       First.
########################################################################

setup_environment() {
    test -n "$SCRIPTS" || SCRIPTS=$HOME/scripts
    test -n "$PRIVATE" || PRIVATE=$HOME/private/scripts
    test -n "$1" || VERSION=4.0
    test -n "$1" && VERSION=$1
    test -n "$2" || TARGET=/opt/glassfish
    test -n "$2" && TARGET=$2
    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
    test "$3" = "sudo" && SUDO=sudo

    case $OSTYPE in
      *darwin*)
        OPTIONS=-Rv
        OWNER=root:wheel
        ;;
      *)
        OPTIONS=-Rvd
        OWNER=root:root
        ;;
    esac
}

add_user() {
    $SUDO useradd --system glassfish -d $TARGET/$VERSION
}

edit_script() {
    $SUDO vim $TARGET/$VERSION/bin/asadmin
    $SUDO vim $TARGET/$VERSION/glassfish/bin/asadmin
}

copy_init() {
    $SUDO cp $OPTIONS $SCRIPTS/etc/init.d/glassfish /etc/init.d/
    $SUDO chown $OWNER /etc/init.d/glassfish
    $SUDO chmod 755 /etc/init.d/glassfish
    $SUDO service glassfish restart
    $SUDO update-rc.d glassfish defaults
}

set_admin_passwd() {
    cd $TARGET/$VERSION/glassfish/bin
    $SUDO -u glassfish sh asadmin change-admin-password
    $SUDO -u glassfish sh asadmin enable-secure-admin
}

set_permission() {
    awk -F':' '{print $1}' /etc/passwd | grep glassfish > /dev/null 2>&1
    if [ $? -eq 0 ]; then
        echo "User glassfish already exist"
    else
        add_user $*
    fi

    $SUDO chown -R glassfish:glassfish $TARGET
    $SUDO chmod -R +x $TARGET/$VERSION/bin/
    $SUDO chmod -R +x $TARGET/$VERSION/glassfish/bin/
    edit_script
    set_admin_passwd
    copy_init
}

install_glassfish() {
    setup_environment $*
    mkdir install_glassfish
    cd install_glassfish
    wget "http://id774.net/java/glassfish-$VERSION.zip"
    unzip glassfish-$VERSION.zip
    mv glassfish4 $VERSION
    test -d $TARGET || $SUDO mkdir $TARGET
    $SUDO cp $OPTIONS $VERSION $TARGET
    test -n "$SUDO" && test -f /etc/issue && set_permission $*
    cd ..
    rm -rf install_glassfish
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_glassfish $*

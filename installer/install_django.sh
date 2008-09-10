#!/bin/sh
#
########################################################################
# Install Django
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 9/8,2008
#       Stable.
########################################################################

install_trunk() {
    test -d /usr/local/src/django || sudo mkdir -p /usr/local/src/django
    cd /usr/local/src/django
    sudo svn co http://code.djangoproject.com/svn/django/trunk/
    cd trunk
    sudo python setup.py install
}

install_branch() {
    test -d /usr/local/src/django || sudo mkdir -p /usr/local/src/django
    cd /usr/local/src/django
    sudo svn co http://code.djangoproject.com/svn/django/tags/releases/$1
    cd $1
    sudo python setup.py install
}

test -n "$1" && install_branch $1
test -n "$1" || install_trunk

case $OSTYPE in
  *darwin*)
    sudo chown -R root:wheel /usr/local/src/django
    ;;
  *)
    sudo chown -R root:root /usr/local/src/django
    ;;
esac

django-admin.py --version

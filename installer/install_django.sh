#!/bin/sh
#
########################################################################
# Install Django
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 2/20,2010
#       Refactoring.
#  v1.0 9/8,2008
#       Stable.
########################################################################

install_trunk() {
    test -d /usr/local/src/django || sudo mkdir -p /usr/local/src/django
    cd /usr/local/src/django
    sudo svn co http://code.djangoproject.com/svn/django/trunk/
    cd trunk
    sudo python setup.py install
    sudo chown -R $OWNER /usr/local/src/django/trunk
}

install_branch() {
    test -d /usr/local/src/django || sudo mkdir -p /usr/local/src/django
    cd /usr/local/src/django
    sudo svn co http://code.djangoproject.com/svn/django/tags/releases/$1
    cd $1
    sudo python setup.py install
    sudo chown -R $OWNER /usr/local/src/django/$1
}

case $OSTYPE in
  *darwin*)
    OWNER=root:wheel
    ;;
  *)
    OWNER=root:root
    ;;
esac

test -n "$1" && install_branch $1
test -n "$1" || install_trunk

django-admin.py --version

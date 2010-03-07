#!/bin/sh
#
########################################################################
# Install Django
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.3 3/7,2010
#       Refactoring.
#  v1.2 2/23,2010
#       Implement svn up and build.
#  v1.1 2/20,2010
#       Refactoring.
#  v1.0 9/8,2008
#       Stable.
########################################################################

install_trunk() {
    if [ -d /usr/local/src/django/trunk ]; then
        cd /usr/local/src/django/trunk
        sudo svn up
    else
        test -d /usr/local/src/django || sudo mkdir -p /usr/local/src/django
        cd /usr/local/src/django
        sudo svn co http://code.djangoproject.com/svn/django/trunk/
        cd trunk
    fi
    sudo python setup.py install
    svn info
    sudo chown -R $OWNER /usr/local/src/django/trunk
}

install_branch() {
    if [ -d /usr/local/src/django/$1 ]; then
        cd /usr/local/src/django/$1
        svn info
    else
        sudo svn co http://svn.ruby-lang.org/repos/ruby/trunk trunk
        cd trunk
        test -d /usr/local/src/django || sudo mkdir -p /usr/local/src/django
        cd /usr/local/src/django
        sudo svn co http://code.djangoproject.com/svn/django/tags/releases/$1
        cd $1
        sudo python setup.py install
        sudo chown -R $OWNER /usr/local/src/django/$1
    fi
}

setup_environment() {
    case $OSTYPE in
      *darwin*)
        OWNER=root:root
        ;;
      *)
        OWNER=root:wheel
        ;;
    esac
}

install_django() {
    setup_environment
    test -n "$1" && install_branch $1
    test -n "$1" || install_trunk
    django-admin.py --version
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
install_django $*

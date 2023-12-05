#!/bin/sh
#
########################################################################
# Install Django
#  $1 = python path
#  $2 = django version
#  $3 = django minor version
#  $4 = not save to src
#  $5 = nosudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v2.0 2013-10-24
#       Install for stable version.
#  v1.3 2010-03-07
#       Refactoring.
#  v1.2 2010-02-23
#       Implement svn up and build.
#  v1.1 2010-02-20
#       Refactoring.
#  v1.0 2008-09-08
#       Stable.
########################################################################

setup_environment() {
    test -n "$1" || PYTHON_PATH=/usr/bin/python
    test -n "$1" && PYTHON_PATH=$1
    test -n "$2" || DJANGO_VERSION=1.6
    test -n "$2" && DJANGO_VERSION=$2
    test -n "$3" || DJANGO_MINOR_VERSION=1.6
    test -n "$3" && DJANGO_MINOR_VERSION=$3
    test -n "$5" || SUDO=sudo
    test -n "$5" && SUDO=
    test "$5" = "sudo" && SUDO=sudo

    case $OSTYPE in
      *darwin*)
        OWNER=root:wheel
        OPTIONS=-pR
        ;;
      *)
        OWNER=root:root
        OPTIONS=-a
        ;;
    esac
}

save_sources() {
    test -d /usr/local/src/django || $SUDO mkdir -p /usr/local/src/django
    $SUDO cp $OPTIONS Django-$DJANGO_VERSION /usr/local/src/django
    sudo chown -R $OWNER /usr/local/src/django
}

install_stable() {
    mkdir install_django
    cd install_django

    wget https://www.djangoproject.com/m/releases/$DJANGO_MINOR_VERSION/Django-$DJANGO_VERSION.tar.gz
    tar xzvf Django-$DJANGO_VERSION.tar.gz
    cd Django-$DJANGO_VERSION
    $SUDO $PYTHON_PATH setup.py install
    cd ..

    test -n "$4" || save_sources
    cd ..
    $SUDO rm -rf install_django
}

install_django() {
    setup_environment $*
    install_stable $*
    django-admin.py --version
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_django $*

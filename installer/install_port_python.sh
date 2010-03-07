#!/bin/sh
#
########################################################################
# MacPorts Python installer
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 3/7,2010
#       Refactoring.
#  v1.1 10/20,2008
#       Add pastedeploy, yaml.
#  v1.0 10/19,2008
#       Stable.
########################################################################

main() {
    sudo port -d selfupdate

    sudo port -d install python25
    sudo ln -fs /opt/local/bin/python2.5 /usr/local/bin/python
    sudo port -d install py25-hashlib
    sudo port -d install py25-zlib
    sudo port -d install py25-setuptools
    sudo ln -fs /opt/local/bin/easy_install-2.5 /usr/local/bin/easy_install

    sudo port -d install py25-readline
    sudo port -d install py25-mysql
    sudo port -d install py25-twisted
    sudo port -d install py25-pgsql
    sudo port -d install py25-mechanize
    sudo port -d install py25-openssl
    sudo port -d install py25-paste
    sudo port -d install py25-pastedeploy
    sudo port -d install py25-simplejson
    sudo port -d install py25-nose
    sudo port -d install py25-sqlalchemy
    sudo port -d install py25-sqlalchemy-migrate
    sudo port -d install py25-turbogears
    sudo port -d install py25-memcached
    sudo port -d install py25-yaml

    port installed
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
main

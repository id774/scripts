#!/bin/sh
#
########################################################################
# MacPorts Python installer
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 10/20,2008
#       Add pastedeploy, yaml.
#  v1.0 10/19,2008
#       Stable.
########################################################################

sudo port -d selfupdate

sudo port -d install python25
sudo ln -s /opt/local/bin/python2.5 /usr/local/bin/python
sudo port -d install py25-hashlib
sudo port -d install py25-zlib
sudo port -d install py25-setuptools
sudo ln -s /opt/local/bin/easy_install-2.5 /usr/local/bin/easy_install

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

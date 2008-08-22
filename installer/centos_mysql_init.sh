#!/bin/sh
#
########################################################################
# Install MySQL Configuration for CentOS 5
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/16,2008
#       Stable.
########################################################################

/etc/init.d/mysqld stop
rm -rf /var/log/mysql
rm -rf /var/log/mysql_innodb
rm -rf /var/lib/mysql
mkdir /var/log/mysql
mkdir /var/log/mysql_innodb
mkdir /var/lib/mysql
mkdir /var/lib/mysql/data
chown mysql:mysql /var/log/mysql
chown mysql:mysql /var/log/mysql_innodb
chown mysql:mysql /var/lib/mysql
chown mysql:mysql /var/lib/mysql/data
chmod 700 /var/log/mysql
chmod 701 /var/lib/mysql
chmod 700 /var/lib/mysql/data
usermod -s /sbin/nologin mysql
mv /var/log/mysqld.log /var/log/mysql/
cp $SCRIPTS/etc/my-centos.cnf /etc/my.cnf
mysql_install_db --defaults-file=/etc/my.cnf
/etc/init.d/mysqld start

#!/bin/sh
#
########################################################################
# Install MySQL Configuration for CentOS 5
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 9/1,2008
#       Improvement permission of mysql, run with sudo.
#  v1.0 8/16,2008
#       Stable.
########################################################################

sudo /etc/init.d/mysqld stop
sudo rm -rf /var/log/mysql
sudo rm -rf /var/log/mysql_innodb
sudo rm -rf /var/lib/mysql
sudo mkdir /var/log/mysql
sudo mkdir /var/log/mysql_innodb
sudo mkdir /var/lib/mysql
sudo mkdir /var/lib/mysql/data
sudo chown mysql:mysql /var/log/mysql
sudo chown mysql:mysql /var/log/mysql_innodb
sudo chown mysql:mysql /var/lib/mysql
sudo chown mysql:mysql /var/lib/mysql/data
sudo chmod 700 /var/log/mysql
sudo chmod 755 /var/lib/mysql
sudo touch /var/lib/mysql/mysql.sock
sudo chmod 700 /var/lib/mysql/data
sudo usermod -s /sbin/nologin mysql
sudo mv /var/log/mysqld.log /var/log/mysql/
sudo cp $SCRIPTS/etc/my-centos.cnf /etc/my.cnf
sudo mysql_install_db --defaults-file=/etc/my.cnf
sudo /etc/init.d/mysqld start

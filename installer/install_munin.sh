#!/bin/sh
#
########################################################################
# Install munin
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 2019-08-22
#       Change default filename of rsyslog.
#  v0.2 2016-12-09
#       Apply munin apache conf template.
#  v0.1 2011-07-07
#       First.
########################################################################

# Packages
sudo apt-get -y install munin munin-node

# Configure
sudo vi /etc/munin/munin.conf /etc/munin/munin-conf.d/localhost.conf
test -r /etc/apache2/.htpasswd || sudo htpasswd -c /etc/apache2/.htpasswd admin
sudo chown root:www-data /etc/apache2/.htpasswd
sudo chmod 640 /etc/apache2/.htpasswd
#*.*;auth,authpriv.none,cron.none,mail.none		-/var/log/syslog
sudo vi /etc/rsyslog.conf /etc/rsyslog.d/50-default.conf
sudo cp $SCRIPTS/etc/munin-apache.conf /etc/munin/apache.conf
sudo chown root:root /etc/munin/apache.conf
test -f /etc/munin/apache24.conf && sudo rm -f /etc/munin/apache24.conf && sudo ln -s /etc/munin/apache.conf /etc/munin/apache24.conf

# Restart
sudo /etc/init.d/rsyslog restart
sudo /etc/init.d/munin-node restart
sudo /etc/init.d/apache2 restart

$SCRIPTS/munin_plugins_links.sh

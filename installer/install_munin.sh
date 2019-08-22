#!/bin/sh
#
########################################################################
# Install munin
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 8/22,2019
#       Change default filename of rsyslog.
#  v0.2 12/9,2016
#       Apply munin apache conf template.
#  v0.1 7/7,2011
#       First.
########################################################################

# Packages
sudo apt-get -y install munin munin-node

# Plugins
test -L /etc/munin/plugins/apt              || sudo ln -s /usr/share/munin/plugins/apt              /etc/munin/plugins/
test -L /etc/munin/plugins/ntp_             || sudo ln -s /usr/share/munin/plugins/ntp_             /etc/munin/plugins/
test -L /etc/munin/plugins/hddtemp_smartctl || sudo ln -s /usr/share/munin/plugins/hddtemp_smartctl /etc/munin/plugins/

# Configure
sudo vi /etc/munin/munin.conf
test -r /etc/apache2/.htpasswd || sudo htpasswd -c /etc/apache2/.htpasswd admin
sudo chown root:www-data /etc/apache2/.htpasswd
sudo chmod 640 /etc/apache2/.htpasswd
#*.*;auth,authpriv.none,cron.none,mail.none		-/var/log/syslog
sudo vi /etc/rsyslog.conf /etc/rsyslog.d/50-default.conf
#AllowOverride All
#Allow from all
sudo cp $SCRIPTS/etc/munin-apache.conf /etc/munin/apache.conf
sudo chown root:root /etc/munin/apache.conf
test -f /etc/munin/apache24.conf && sudo rm -f /etc/munin/apache24.conf && sudo ln -s /etc/munin/apache.conf /etc/munin/apache24.conf

# Restart
sudo /etc/init.d/rsyslog restart
sudo /etc/init.d/munin-node restart
sudo /etc/init.d/apache2 restart

munin-node-configure

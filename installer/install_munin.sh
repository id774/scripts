#!/bin/sh
#
########################################################################
# Install munin
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 7/7,2011
#       First.
########################################################################

# Packages
sudo apt-get install munin munin-node

# Plugins
sudo ln -s /usr/share/munin/plugins/apt              /etc/munin/plugins/
sudo ln -s /usr/share/munin/plugins/ntp_             /etc/munin/plugins/
sudo ln -s /usr/share/munin/plugins/hddtemp_smartctl /etc/munin/plugins/

# Configure
sudo vim /etc/munin/munin.conf
test -r /etc/apache2/.htpasswd || sudo htpasswd -c /etc/apache2/.htpasswd admin
sudo chown root:www-data /etc/apache2/.htpasswd
sudo chmod 640 /etc/apache2/.htpasswd
#*.*;auth,authpriv.none,cron.none,mail.none		-/var/log/syslog
sudo vim /etc/rsyslog.conf
#AllowOverride All
#Allow from all
sudo vim /etc/munin/apache.conf

# Restart
sudo /etc/init.d/rsyslog restart
sudo /etc/init.d/munin-node restart
sudo /etc/init.d/apache2 restart

munin-node-configure

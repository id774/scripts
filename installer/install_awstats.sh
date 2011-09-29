#!/bin/sh
#
########################################################################
# Install awstats
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 9/7,2011
#       First.
########################################################################

# Packages
sudo apt-get install awstats

# Configure
sudo vi /etc/awstats/awstats.conf*
sudo vi /etc/apache2/sites-available/default*
sudo vi /etc/logrotate.d/apache2
sudo chmod 440 /var/log/apache2/*
sudo chown www-data:adm /var/log/apache2/*
sudo chmod 550 /var/log/apache2
sudo chown www-data:adm /var/log/apache2

# Restart
sudo /etc/init.d/apache2 restart
sudo -u www-data /usr/lib/cgi-bin/awstats.pl -config=awstats -update

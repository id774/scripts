#!/bin/sh

test -d /etc/apache2/ssl || sudo mkdir -p /etc/apache2/ssl
sudo /usr/sbin/make-ssl-cert /usr/share/ssl-cert/ssleay.cnf /etc/apache2/ssl/apache.pem

sudo cp $SCRIPTS/etc/apache/custom /etc/apache2/sites-available/
sudo cp $SCRIPTS/etc/apache/custom-ssl /etc/apache2/sites-available/
sudo chmod 644 /etc/apache2/sites-available/custom
sudo chmod 644 /etc/apache2/sites-available/custom-ssl
sudo vi /etc/apache2/sites-available/custom
sudo vi /etc/apache2/sites-available/custom-ssl

sudo a2enmod ssl
sudo a2dissite default
sudo a2dissite default-ssl
sudo a2ensite custom
sudo a2ensite custom-ssl
sudo /etc/init.d/apache2 reload

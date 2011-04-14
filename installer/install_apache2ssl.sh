#!/bin/sh

test -d /etc/apache2/ssl || sudo mkdir -p /etc/apache2/ssl
sudo /usr/sbin/make-ssl-cert /usr/share/ssl-cert/ssleay.cnf /etc/apache2/ssl/apache.pem
sudo vim /etc/apache2/sites-available/default-ssl /etc/apache2/sites-available/default
sudo a2enmod ssl
sudo a2ensite default-ssl
sudo /etc/init.d/apache2 reload

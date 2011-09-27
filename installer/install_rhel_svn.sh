#!/bin/sh
#
########################################################################
# Install Subversion Server for RHEL
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 9/27,2011
#       First.
########################################################################

SVN_PROJECT_ID=default

# Subversion Install
sudo yum -y install subversion mod_dav_svn

# Make Repository
sudo mkdir -p /var/lib/svn/$SVN_PROJECT_ID
sudo svnadmin create /var/lib/svn/$SVN_PROJECT_ID/repos

sudo chown -R apache. /var/lib/svn
sudo chmod 770 /var/lib/svn
sudo htpasswd -c /etc/httpd/conf/.htpasswd admin


sudo /etc/rc.d/init.d/httpd restart


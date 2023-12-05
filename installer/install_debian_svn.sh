#!/bin/sh
#
########################################################################
# Install Subversion Server for Debian
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 2011-09-28
#       Fix svn path.
#  v1.1 2011-07-21
#       Remove svn user.
#  v1.0 2008-08-15
#       Stable.
########################################################################

SVN_PROJECT_ID=project

# Subversion Install
sudo apt-get -y install apache2
sudo update-rc.d -f apache-perl remove
sudo update-rc.d -f apache2 defaults
sudo apt-get -y install subversion
sudo apt-get -y install libapache2-svn
sudo apt-get -y install libapache-mod-dav

# Make Repository
sudo mkdir -p /var/lib/svn
sudo svnadmin create /var/lib/svn/$SVN_PROJECT_ID

# Configuration
sudo vi /etc/apache2/mods-enabled/dav_svn.conf
#<Location /svn/$SVN_PROJECT_ID>
#  DAV svn
#  SVNPath /var/lib/svn/$SVN_PROJECT_ID/repos
#  AuthType Basic
#  AuthName "Subversion Repository"
#  AuthUserFile /etc/apache2/.htpasswd
#  <LimitExcept GET PROPFIND OPTIONS REPORT>
#    Require valid-user
#  </LimitExcept>
#</Location>

# Permission
sudo chown -R www-data:www-data /var/lib/svn
sudo chmod -R 770 /var/lib/svn

# Apache2 Reload
sudo /etc/init.d/apache2 reload

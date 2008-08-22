#!/bin/sh
#
########################################################################
# Install Subversion Server
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

SVN_PROJECT_ID=sample

# Subversion Install
sudo aptitude -y install apache2
sudo update-rc.d -f apache-perl remove
sudo update-rc.d -f apache2 defaults
sudo aptitude -y install subversion
sudo aptitude -y install libapache2-svn
sudo aptitude -y install libapache-mod-dav

# Delete Old Repository
sudo test -d /var/lib/svn/$SVN_PROJECT_ID && sudo rm -rf /var/lib/svn/$SVN_PROJECT_ID

# Make Repository
sudo mkdir -p /var/lib/svn/$SVN_PROJECT_ID
sudo svnadmin create /var/lib/svn/$SVN_PROJECT_ID/repos

# Configuration
sudo vim /etc/apache2/mods-enabled/dav_svn.conf
#<Location /svn/$SVN_PROJECT_ID>
#  DAV svn
#  SVNPath /var/lib/svn/$SVN_PROJECT_ID/repos
#  AuthType Basic
#  AuthName "Subversion Repository"
#  AuthUserFile /var/lib/trac/$SVN_PROJECT_ID/.htpasswd
#  <LimitExcept GET PROPFIND OPTIONS REPORT>
#    Require valid-user
#  </LimitExcept> 
#</Location>

# Import Dump
test -f ~/$SVN_PROJECT_ID.dump && sudo svnadmin load /var/lib/svn/$SVN_PROJECT_ID/repos < ~/$SVN_PROJECT_ID.dump

# Access Control
sudo htpasswd -c /var/lib/svn/$SVN_PROJECT_ID/.htpasswd admin

# Permission
sudo useradd svn
sudo chsh -s /bin/false svn
sudo chown -R svn:svn /var/lib/svn
sudo chmod -R 770 /var/lib/svn

# Apache2 Reload
sudo /etc/init.d/apache2 reload


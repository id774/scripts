#!/bin/sh
#
########################################################################
# Install Trac and Subversion Server
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 8/25,2008
#       Edit group, pass test.
#  v1.0 8/15,2008
#       stable.
########################################################################

test -n "$1" && TRAC_PROJECT_ID=$1
test -n "$1" || TRAC_PROJECT_ID=default

# Install Package
sudo aptitude -y install apache2
sudo aptitude -y install libapache2-mod-python
sudo update-rc.d -f apache-perl remove
sudo update-rc.d -f apache2 defaults
sudo aptitude -y install subversion
sudo aptitude -y install libapache2-svn 
sudo aptitude -y install libapache-mod-dav
sudo aptitude -y install sqlite3
sudo aptitude -y install sqlite
sudo aptitude -y install python-sqlite
sudo aptitude -y install python-pysqlite2
sudo aptitude -y install python-pysqlite1.1
sudo aptitude -y install trac
sudo aptitude -y install trac-ja-resource
sudo aptitude -y install python-svn

# Delete Old Repository
sudo test -d /var/lib/trac/$TRAC_PROJECT_ID && sudo rm -rf /var/lib/trac/$TRAC_PROJECT_ID
sudo test -d /var/lib/svn/$TRAC_PROJECT_ID && sudo rm -rf /var/lib/svn/$TRAC_PROJECT_ID

# Make Repository
sudo mkdir -p /var/lib/trac/$TRAC_PROJECT_ID
sudo mkdir -p /var/lib/svn/$TRAC_PROJECT_ID
sudo svnadmin create /var/lib/svn/$TRAC_PROJECT_ID/repos
sudo trac-admin /var/lib/trac/$TRAC_PROJECT_ID initenv
sudo trac-admin /var/lib/trac/$TRAC_PROJECT_ID resync

# Configuration
sudo vim /etc/apache2/mods-enabled/dav_svn.conf
#<Location /svn/$TRAC_PROJECT_ID>
#  DAV svn
#  SVNPath /var/lib/svn/$TRAC_PROJECT_ID/repos
#  AuthType Basic
#  AuthName "Subversion Repository"
#  AuthUserFile /var/lib/svn/$TRAC_PROJECT_ID/.htpasswd
#  Require valid-user
#</Location>
sudo vim /var/lib/trac/$TRAC_PROJECT_ID/conf/trac.ini
#repository_dir = /var/lib/svn/$TRAC_PROJECT_ID/repos
#default_charset = utf8
sudo vim /etc/apache2/sites-available/$TRAC_PROJECT_ID
#    Alias /trac/ "/usr/share/trac/htdocs/"
#    ScriptAlias /$TRAC_PROJECT_ID /usr/share/trac/cgi-bin/trac.cgi
#    <Location "/$TRAC_PROJECT_ID">
#      SetEnv TRAC_ENV "/var/lib/trac/$TRAC_PROJECT_ID"
#    </Location>
#    <LocationMatch "/[^/]+/login">
#      AuthType Basic
#      AuthName "Subversion Repository"
#      AuthUserFile /var/lib/svn/$TRAC_PROJECT_ID/.htpasswd
#      Require valid-user
#    </LocationMatch>

# Import Dump
test -f ~/$TRAC_PROJECT_ID.dump && sudo svnadmin load /var/lib/svn/$TRAC_PROJECT_ID/repos < ~/$TRAC_PROJECT_ID.dump
test -f ~/$TRAC_PROJECT_ID.dump && sudo trac-admin /var/lib/trac/$TRAC_PROJECT_ID resync
test -f ~/$TRAC_PROJECT_ID.db && sudo cp $TRAC_PROJECT_ID.db /var/lib/trac/$TRAC_PROJECT_ID/db/trac.db
test -f ~/$TRAC_PROJECT_ID.db && sudo trac-admin /var/lib/trac/$TRAC_PROJECT_ID upgrade

# Access Control
sudo htpasswd -c /var/lib/svn/$TRAC_PROJECT_ID/.htpasswd admin
sudo trac-admin /var/lib/trac/$TRAC_PROJECT_ID/ permission add admin TRAC_ADMIN

# Permission
sudo useradd svn
sudo chsh -s /bin/false svn
sudo chown -R svn:svn /var/lib/svn
sudo chown -R www-data:www-data /var/lib/trac
sudo chmod -R 770 /var/lib/svn
sudo chmod -R 770 /var/lib/trac

# Edit Group
sudo vim /etc/group

# Test
#sudo tracd --basic-auth $TRAC_PROJECT_ID,/var/lib/svn/$TRAC_PROJECT_ID/.htpasswd,$TRAC_PROJECT_ID -p 4080 /var/lib/trac/$TRAC_PROJECT_ID/

# Apache2 Restart
sudo /etc/init.d/apache2 restart


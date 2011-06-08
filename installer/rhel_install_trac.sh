#!/bin/sh
#
########################################################################
# Install Trac and Subversion Server for RHEL
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 8/25,2008
#       Edit group, pass test.
#  v1.0 8/16,2008
#       Stable.
########################################################################

TRAC_PROJECT_ID=default

# Install Package
sudo yum -y install httpd
sudo chkconfig httpd on
sudo yum -y install mod_dav_svn
sudo yum -y install mod_python
sudo yum -y install subversion

# Install DAG
wget http://dag.wieers.com/rpm/packages/rpmforge-release/rpmforge-release-0.3.6-1.el5.rf.i386.rpm
sudo rpm -Uvh rpmforge-release-0.3.6-1.el5.rf.i386.rpm
sudo vim /etc/yum.repos.d/rpmforge.repo
# enabled = 0
sudo yum --enablerepo=rpmforge install -y python-clearsilver

# Install Trac
wget http://www.i-act.co.jp/project/products/downloads/trac-0.10.4-ja-1.zip
unzip trac-0.10.4-ja-1.zip
cd trac-0.10.4-ja-1
sudo python setup.py install
cd ..
sudo rm -rf trac-0.10.4-ja-1

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
sudo vim /etc/httpd/conf.d/subversion.conf
#<Location /svn/$TRAC_PROJECT_ID>
#  DAV svn
#  SVNPath /var/lib/svn/$TRAC_PROJECT_ID/repos
#  AuthType Basic
#  AuthName "Subversion Repository"
#  AuthUserFile /var/lib/svn/$TRAC_PROJECT_ID/.htpasswd
#  <LimitExcept GET PROPFIND OPTIONS REPORT>
#    Require valid-user
#  </LimitExcept> 
#</Location>
sudo vim /var/lib/trac/$TRAC_PROJECT_ID/conf/trac.ini
#repository_dir = /var/lib/svn/$TRAC_PROJECT_ID/repos
sudo vim /etc/httpd/conf/httpd.conf
#    Alias /trac/ "/usr/share/trac/htdocs/"
#    ScriptAlias /$TRAC_PROJECT_ID /usr/share/trac/cgi-bin/trac.cgi
#    <Location "/$TRAC_PROJECT_ID">
#      SetEnv TRAC_ENV "/var/lib/trac/$TRAC_PROJECT_ID"
#    </Location>

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
sudo chown -R apache:apache /var/lib/trac
sudo chmod -R 770 /var/lib/svn
sudo chmod -R 770 /var/lib/trac

# Edit Group
sudo vim /etc/group

# Test
#sudo tracd --basic-auth $TRAC_PROJECT_ID,/var/lib/svn/$TRAC_PROJECT_ID/.htpasswd,$TRAC_PROJECT_ID -p 4080 /var/lib/trac/$TRAC_PROJECT_ID/

# Apache2 Restart
sudo service httpd restart


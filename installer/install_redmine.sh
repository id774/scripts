#!/bin/sh
#
########################################################################
# Install RedMine
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

test -n "$1" && SVN_PROJECT_ID=$1
test -n "$1" || SVN_PROJECT_ID=redmine
RAILS_ENV=production
RAILS_ROOT=/opt/rails/redmine

# Delete Old Repository
sudo test -d /var/lib/svn/$SVN_PROJECT_ID && sudo rm -rf /var/lib/svn/$SVN_PROJECT_ID
sudo test -d /opt/rails/redmine && sudo rm -rf /opt/rails/redmine

# Checkout
sudo mkdir -p /opt/rails/redmine
cd /opt/rails
sudo svn co svn://rubyforge.org/var/svn/redmine/trunk redmine
cd /opt/rails/redmine
sudo cp config/database.yml.example config/database.yml
sudo vi config/database.yml
#production:
#  adapter: mysql
#  database: redmine
#  username: redmine
#  password: PASSWORD
#  socket: /var/run/mysqld/mysqld.sock
#  encoding: utf8

# Database
mysql -u root -p
# create database redmine character set utf8;
# grant all on redmine.* to redmine@localhost identified by 'PASSWORD';
# exit

# Migrate
cd /opt/rails/redmine
sudo rake db:migrate RAILS_ENV=$RAILS_ENV
sudo rake load_default_data RAILS_ENV=$RAILS_ENV

# Setup Environment
sudo vi config/environment.rb

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
#  AuthUserFile /var/lib/svn/$SVN_PROJECT_ID/.htpasswd
#  <LimitExcept GET PROPFIND OPTIONS REPORT>
#    Require valid-user
#  </LimitExcept>
#</Location>

# Mongrel Cluster
cd /opt/rails/redmine
test $RAILS_ENV = production && sudo mongrel_rails cluster::configure -e production -p 3080 -a 0.0.0.0 -l /opt/rails/redmine/log/mongrel.log -P /opt/rails/redmine/tmp/redmine.pid -c /opt/rails/redmine -r /opt/rails/redmine -N 1 --user rails --group rails

# Permission
sudo useradd svn
sudo chsh -s /bin/false svn
sudo chown -R svn:svn /var/lib/svn
sudo chmod -R 770 /var/lib/svn
sudo useradd rails -G svn
sudo chsh -s /bin/false rails
sudo chown -R rails:rails /opt/rails
sudo chmod -R 770 /opt/rails

# Apache2 Reload
sudo /etc/init.d/apache2 reload

# Start Mongrel
#mongrel_rails cluster::start

# Startup
test -f $SCRIPTS/redmine && sudo cp $SCRIPTS/redmine /etc/init.d/redmine
sudo chmod 755 /etc/init.d/redmine
sudo update-rc.d redmine defaults


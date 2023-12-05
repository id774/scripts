#!/bin/sh
#
########################################################################
# Install Subversion Server for RHEL
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 2011-09-28
#       Fix svn path.
#  v0.1 2011-09-27
#       First.
########################################################################

SVN_PROJECT_ID=project

# Subversion Install
sudo yum -y install subversion mod_dav_svn

# Make Repository
sudo mkdir -p /var/lib/svn
sudo svnadmin create /var/lib/svn/$SVN_PROJECT_ID

# Permission
sudo chown -R apache. /var/lib/svn
sudo chmod 770 /var/lib/svn
sudo chcon -R -h -u system_u -t httpd_sys_content_t /var/lib/svn
#sudo htpasswd -c /etc/httpd/conf/.htpasswd admin

# Configuration
sudo vi /etc/httpd/conf/httpd.conf
sudo vi /etc/httpd/conf.d/subversion.conf

# Restart
sudo /etc/rc.d/init.d/httpd restart

#!/bin/sh
#
########################################################################
# Server Resource Report Setup Script
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.5 2014-06-06
#       Remove obsolete archive.
#  v1.4 2011-11-05
#       Purge obsolete jobs.
#  v1.3 2008-09-11
#       Split.
#  v1.2 2008-08-21
#       Add clone git2svn job.
#  v1.1 2008-08-18
#       Edit all script after deploy.
#  v1.0 2008-08-15
#       Stable.
########################################################################

# Make Directory
test -d /etc/cron.weekday || sudo mkdir /etc/cron.weekday
test -d /etc/cron.weekend || sudo mkdir /etc/cron.weekend
test -d /var/log/sysadmin || sudo mkdir /var/log/sysadmin
sudo chmod 750 /var/log/sysadmin
sudo chown root:adm /var/log/sysadmin

# Server Resource Report Job
sudo cp $SCRIPTS/get_resources.sh /root/bin/get_resources.sh
sudo chmod 700 /root/bin/get_resources.sh
sudo chown root:root /root/bin/get_resources.sh
sudo cp $SCRIPTS/cron/bin/get_resources /etc/cron.hourly/get_resources
sudo chmod 750 /etc/cron.hourly/get_resources
sudo chown root:adm /etc/cron.hourly/get_resources
sudo touch /var/log/sysadmin/resources.log
sudo chmod 640 /var/log/sysadmin/resources.log
sudo chown root:adm /var/log/sysadmin/resources.log
sudo cp $SCRIPTS/cron/etc/logrotate.d/resources /etc/logrotate.d/resources
sudo chmod 644 /etc/logrotate.d/resources
sudo chown root:root /etc/logrotate.d/resources

# Edit crontab
# 50 23 * * 1-5 root cd / && run-parts --report /etc/cron.weekday
# 55 6  * * 6   root cd / && run-parts --report /etc/cron.weekend
sudo vi /etc/crontab $SCRIPTS/installer/install_get_resources.sh


#!/bin/sh
#
########################################################################
# cronJob Setup Script
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.3 9/11,2008
#       Split.
#  v1.2 8/21,2008
#       Add clone git2svn job.
#  v1.1 8/18,2008
#       Edit all script after deploy.
#  v1.0 8/15,2008
#       Stable.
########################################################################

# Make Directory
test -d /etc/cron.weekday || sudo mkdir /etc/cron.weekday
test -d /etc/cron.weekend || sudo mkdir /etc/cron.weekend
test -d /var/log/sysadmin || sudo mkdir /var/log/sysadmin
sudo chmod 750 /var/log/sysadmin
sudo chown root:adm /var/log/sysadmin

# Daily Backup Job
sudo cp $SCRIPTS/cron/bin/backup.sh /root/bin/backup.sh
sudo vim /root/bin/backup.sh
sudo chmod 700 /root/bin/backup.sh
sudo cp $SCRIPTS/cron/etc/backup_exclude /root/bin/backup_exclude
sudo vim /root/bin/backup_exclude
sudo chmod 600 /root/bin/backup_exclude
sudo chown -R root:root /root/bin
sudo test -d /home/backup || sudo mkdir /home/backup
sudo chmod 750 /home/backup
sudo chown -R root:admin /home/backup
sudo cp $SCRIPTS/cron/bin/backup /etc/cron.daily/backup
sudo vim /etc/cron.daily/backup
sudo chmod 750 /etc/cron.daily/backup
sudo chown root:adm /etc/cron.daily/backup
sudo touch /var/log/sysadmin/backup.log
sudo chmod 640 /var/log/sysadmin/backup.log
sudo chown root:adm /var/log/sysadmin/backup.log
sudo cp $SCRIPTS/cron/etc/logrotate.d/backup /etc/logrotate.d/backup
sudo chmod 644 /etc/logrotate.d/backup
sudo chown root:root /etc/logrotate.d/backup

# Auto Upgrade Job
sudo cp $SCRIPTS/cron/bin/auto-upgrade /etc/cron.daily/auto-upgrade
sudo vim /etc/cron.daily/auto-upgrade
sudo chmod 750 /etc/cron.daily/auto-upgrade
sudo chown root:adm /etc/cron.daily/auto-upgrade
sudo touch /var/log/sysadmin/auto-upgrade.log
sudo chmod 640 /var/log/sysadmin/auto-upgrade.log
sudo chown root:adm /var/log/sysadmin/auto-upgrade.log
sudo cp $SCRIPTS/cron/etc/logrotate.d/auto-upgrade /etc/logrotate.d/auto-upgrade
sudo chmod 644 /etc/logrotate.d/auto-upgrade
sudo chown root:root /etc/logrotate.d/auto-upgrade

# Server Resource Report Job
sudo cp $SCRIPTS/get_resources.sh /root/bin/get_resources.sh
sudo chmod 700 /root/bin/get_resources.sh
sudo chown -R root:root /root/bin
sudo cp $SCRIPTS/cron/bin/get_resources /etc/cron.hourly/get_resources
sudo chmod 750 /etc/cron.hourly/get_resources
sudo chown root:adm /etc/cron.hourly/get_resources
sudo touch /var/log/sysadmin/resources.log
sudo chmod 640 /var/log/sysadmin/resources.log
sudo chown root:adm /var/log/sysadmin/resources.log
sudo cp $SCRIPTS/cron/etc/logrotate.d/resources /etc/logrotate.d/resources
sudo chmod 644 /etc/logrotate.d/resources
sudo chown root:root /etc/logrotate.d/resources

# Archive Old Logs
if [ -f /var/log/auto-upgrade.log ]; then
    test -d /var/log/sysadmin/archive || sudo mkdir /var/log/sysadmin/archive
    sudo chmod 750 /var/log/sysadmin/archive
    sudo chown root:adm /var/log/sysadmin/archive
    test -f /var/log/auto-upgrade.log && sudo mv /var/log/auto-upgrade.log* /var/log/sysadmin/archive/
    test -f /var/log/backup && sudo mv /var/log/backup* /var/log/sysadmin/archive/
    test -f /var/log/resources.log && sudo mv /var/log/resources.log* /var/log/sysadmin/archive/
fi

# Edit crontab
# 50 23 * * 1-5 root cd / && run-parts --report /etc/cron.weekday
# 55 6  * * 6   root cd / && run-parts --report /etc/cron.weekend
sudo vim /etc/crontab $SCRIPTS/installer/install_cronjob.sh


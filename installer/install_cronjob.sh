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
sudo touch /var/log/backup
sudo chmod 640 /var/log/backup
sudo chown root:adm /var/log/backup
sudo cp $SCRIPTS/cron/etc/backup-log /etc/logrotate.d/backup
sudo chmod 644 /etc/logrotate.d/backup
sudo chown root:root /etc/logrotate.d/backup

# Auto Upgrade Job
sudo cp $SCRIPTS/cron/bin/auto-upgrade /etc/cron.daily/auto-upgrade
sudo vim /etc/cron.daily/auto-upgrade
sudo chmod 750 /etc/cron.daily/auto-upgrade
sudo chown root:adm /etc/cron.daily/auto-upgrade
sudo touch /var/log/auto-upgrade.log
sudo chmod 640 /var/log/auto-upgrade.log
sudo chown root:adm /var/log/auto-upgrade.log
sudo cp $SCRIPTS/cron/etc/auto-upgrade-log /etc/logrotate.d/auto-upgrade
sudo chmod 644 /etc/logrotate.d/auto-upgrade
sudo chown root:root /etc/logrotate.d/auto-upgrade

# Server Resource Report Job
sudo cp $SCRIPTS/get_resources.sh /root/bin/get_resources.sh
sudo chmod 700 /root/bin/get_resources.sh
sudo chown -R root:root /root/bin
sudo cp $SCRIPTS/cron/bin/get_resources /etc/cron.hourly/get_resources
sudo chmod 750 /etc/cron.hourly/get_resources
sudo chown root:adm /etc/cron.hourly/get_resources
sudo touch /var/log/resources.log
sudo chmod 640 /var/log/resources.log
sudo chown root:adm /var/log/resources.log
sudo cp $SCRIPTS/cron/etc/resources-log /etc/logrotate.d/resources
sudo chmod 644 /etc/logrotate.d/resources
sudo chown root:root /etc/logrotate.d/resources

# ClamAV Auto Upgrade Job
sudo cp $SCRIPTS/cron/bin/clamav_upgrade.sh /root/bin/clamav_upgrade.sh
sudo chmod 700 /root/bin/clamav_upgrade.sh
sudo chown -R root:root /root/bin
sudo cp $SCRIPTS/cron/bin/clamav_upgrade /etc/cron.monthly/clamav_upgrade
sudo vim /etc/cron.monthly/clamav_upgrade
sudo chmod 750 /etc/cron.monthly/clamav_upgrade
sudo chown root:adm /etc/cron.monthly/clamav_upgrade
sudo touch /var/log/clamav_upgrade
sudo chmod 640 /var/log/clamav_upgrade
sudo chown root:adm /var/log/clamav_upgrade
sudo touch /var/log/clamscan.log
sudo chmod 640 /var/log/clamscan.log
sudo chown root:adm /var/log/clamscan.log
sudo cp $SCRIPTS/cron/etc/clamav_upgrade-log /etc/logrotate.d/clamav_upgrade
sudo chmod 644 /etc/logrotate.d/clamav_upgrade
sudo chown root:root /etc/logrotate.d/clamav_upgrade

# Edit crontab
sudo vim /etc/crontab $SCRIPTS/cron/plagger/crontab


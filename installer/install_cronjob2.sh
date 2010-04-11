#!/bin/sh
#
########################################################################
# cronJob Setup Script 2
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.5 4/11,2010
#       Erase rsync_backup2.
#  v1.4 1/22,2009
#       Fix script path.
#  v1.3 9/11,2008
#       Split.
########################################################################

# Rsync Backup Job
sudo cp $SCRIPTS/cron/bin/rsync_backup.sh /root/bin/rsync_backup.sh
sudo vim /root/bin/rsync_backup.sh
sudo chmod 700 /root/bin/rsync_backup.sh
sudo cp $SCRIPTS/cleanup4mac.sh /root/bin/cleanup4mac.sh
sudo chmod 700 /root/bin/cleanup4mac.sh
sudo chown -R root:root /root/bin
sudo cp $SCRIPTS/cron/bin/rsync_backup /etc/cron.hourly/rsync_backup
sudo vim /etc/cron.hourly/rsync_backup
sudo chmod 750 /etc/cron.hourly/rsync_backup
sudo chown root:adm /etc/cron.hourly/rsync_backup
sudo touch /var/log/rsync_backup
sudo chmod 640 /var/log/rsync_backup
sudo chown root:adm /var/log/rsync_backup
sudo cp $SCRIPTS/cron/etc/rsync_backup-log /etc/logrotate.d/rsync_backup
sudo chmod 644 /etc/logrotate.d/rsync_backup
sudo chown root:root /etc/logrotate.d/rsync_backup

# Edit crontab
sudo vim /etc/crontab $SCRIPTS/cron/plagger/crontab


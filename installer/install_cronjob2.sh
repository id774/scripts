#!/bin/sh
#
########################################################################
# cronJob Setup Script 2
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.6 8/9,2010
#       Setup github-arc script.
#  v1.5 4/11,2010
#       Erase rsync_backup2.
#  v1.4 1/22,2009
#       Fix script path.
#  v1.3 9/11,2008
#       Split.
########################################################################

# Make Directory
test -d /etc/cron.weekday || sudo mkdir /etc/cron.weekday
test -d /etc/cron.weekend || sudo mkdir /etc/cron.weekend
test -d /var/log/sysadmin || sudo mkdir /var/log/sysadmin
sudo chmod 750 /var/log/sysadmin
sudo chown root:adm /var/log/sysadmin

# Rsync Backup Job
sudo cp $SCRIPTS/cron/bin/rsync_backup.sh /root/bin/rsync_backup.sh
sudo vim /root/bin/rsync_backup.sh
sudo chmod 700 /root/bin/rsync_backup.sh
sudo cp $SCRIPTS/cleanup4mac.sh /root/bin/cleanup4mac.sh
sudo chmod 700 /root/bin/cleanup4mac.sh
sudo cp $SCRIPTS/github-arc.sh /root/bin/github-arc.sh
sudo chmod 700 /root/bin/github-arc.sh
sudo chown -R root:root /root/bin
sudo cp $SCRIPTS/cron/bin/rsync_backup /etc/cron.hourly/rsync_backup
sudo vim /etc/cron.hourly/rsync_backup
sudo chmod 750 /etc/cron.hourly/rsync_backup
sudo chown root:adm /etc/cron.hourly/rsync_backup
sudo touch /var/log/rsync_backup
sudo chmod 640 /var/log/rsync_backup
sudo chown root:adm /var/log/rsync_backup
sudo cp $SCRIPTS/cron/etc/logrotate.d/rsync_backup /etc/logrotate.d/rsync_backup
sudo chmod 644 /etc/logrotate.d/rsync_backup
sudo chown root:root /etc/logrotate.d/rsync_backup

# Archive Old Logs
if [ -f /var/log/rsync_backup.log ]; then
    test -d /var/log/sysadmin/archive || sudo mkdir /var/log/sysadmin/archive
    sudo chmod 750 /var/log/sysadmin/archive
    sudo chown root:adm /var/log/sysadmin/archive
    test -f /var/log/rsync_backup && sudo mv /var/log/rsync_backup.log* /var/log/sysadmin/archive/
fi

# Edit crontab
# 50 23 * * 1-5 root cd / && run-parts --report /etc/cron.weekday
# 55 6  * * 6   root cd / && run-parts --report /etc/cron.weekend
sudo vim /etc/crontab


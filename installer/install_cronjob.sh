#!/bin/sh
#
########################################################################
# cronJob Setup Script
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
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

# Plagger
sudo useradd -m plagger
sudo passwd plagger
sudo chsh -s /bin/false plagger
sudo mkdir -p /home/plagger/.plagger_tmp/tmp1
sudo mkdir -p /home/plagger/.plagger_tmp/tmp2
sudo mkdir -p /home/plagger/.plagger_tmp/tmp3
sudo mkdir -p /home/plagger/.plagger_tmp/tmp4
sudo mkdir -p /home/plagger/.plagger_tmp/tmp5
sudo mkdir -p /home/plagger/bin
sudo mkdir -p /home/plagger/yaml
sudo chmod -R 750 /home/plagger
sudo cp $SCRIPTS/cron/plagger/bin/* /home/plagger/bin/
sudo vim /home/plagger/bin/*
sudo chmod 750 /home/plagger/bin/*
sudo cp $SCRIPTS/cron/plagger/yaml/* /home/plagger/yaml/
sudo vim /home/plagger/yaml/*
sudo chmod 640 /home/plagger/yaml/*
sudo cp $SCRIPTS/cron/plagger-log /etc/logrotate.d/plagger
sudo chmod 644 /etc/logrotate.d/plagger
sudo chown root:root /etc/logrotate.d/plagger
sudo touch /var/log/plagger.log
sudo touch /var/log/plaggersbm.log
sudo chmod 640 /var/log/plagger.log
sudo chmod 640 /var/log/plaggersbm.log
sudo chown plagger:adm /var/log/plagger.log
sudo chown plagger:adm /var/log/plaggersbm.log
sudo chown -R plagger:plagger /home/plagger

# Twitter
sudo cp $SCRIPTS/cron/etc/last_id /home/plagger/.plagger_tmp/last_id
sudo chmod 640 /home/plagger/.plagger_tmp/last_id
sudo cp $SCRIPTS/cron/bin/twitter-client.py /home/plagger/bin/twitter-client.py
sudo vim /home/plagger/bin/twitter-client.py
sudo chmod 750 /home/plagger/bin/twitter-client.py
sudo cp $SCRIPTS/cron/bin/twitter-logger /home/plagger/bin/twitter-logger
sudo vim /home/plagger/bin/twitter-logger
sudo chmod 750 /home/plagger/bin/twitter-logger
sudo cp $SCRIPTS/cron/etc/twitter-log /etc/logrotate.d/twitter-log
sudo chmod 644 /etc/logrotate.d/twitter-log
sudo chown root:root /etc/logrotate.d/twitter-log
sudo touch /var/log/twitter.log
sudo chmod 640 /var/log/twitter.log
sudo chown plagger:adm /var/log/twitter.log
sudo touch /var/log/twitter-update.log
sudo chmod 640 /var/log/twitter-update.log
sudo chown plagger:adm /var/log/twitter-update.log
sudo chown -R plagger:plagger /home/plagger

# Mixi
sudo cp $SCRIPTS/mixi.py /usr/lib/python2.5/site-packages/mixi.py
sudo cp $SCRIPTS/cron/bin/mixidiary2txt.py /home/plagger/bin/mixidiary2txt.py
sudo vim /home/plagger/bin/mixidiary2txt.py
sudo chmod 750 /home/plagger/bin/mixidiary2txt.py
sudo cp $SCRIPTS/cron/bin/mixi-logger /home/plagger/bin/mixi-logger
sudo vim /home/plagger/bin/mixi-logger
sudo chmod 750 /home/plagger/bin/mixi-logger
sudo cp $SCRIPTS/cron/etc/mixi-log /etc/logrotate.d/mixi-log
sudo chmod 644 /etc/logrotate.d/mixi-log
sudo chown root:root /etc/logrotate.d/mixi-log
sudo touch /var/log/mixi.log
sudo chmod 640 /var/log/mixi.log
sudo chown plagger:adm /var/log/mixi.log
sudo touch /var/log/mixi-update.log
sudo chmod 640 /var/log/mixi-update.log
sudo chown plagger:adm /var/log/mixi-update.log
sudo chown -R plagger:plagger /home/plagger

# Clone GitHub to SVN Job (needs to setup git and svk by root)
sudo mkdir -p /root/local/github
# (root) git clone git://github.com/id774/scripts.git
sudo mkdir -p /root/svnwork
# (root) svk mirror http://svn.assembla.com/svn/id774 //mirror/id774
# (root) svk sync //mirror/id774
# (root) svk cp -m "Initial" //mirror/id774 //id774
# (root) svk co //id774
sudo cp $SCRIPTS/cron/bin/clone_git2svn.sh /root/bin/clone_git2svn.sh
sudo vim /root/bin/clone_git2svn.sh
sudo chmod 700 /root/bin/clone_git2svn.sh
sudo cp $SCRIPTS/cron/bin/clone_git2svn /etc/cron.hourly/clone_git2svn
sudo vim /etc/cron.hourly/clone_git2svn
sudo chmod 750 /etc/cron.hourly/clone_git2svn
sudo chown root:adm /etc/cron.hourly/clone_git2svn
sudo touch /var/log/clone_git2svn.log
sudo chmod 640 /var/log/clone_git2svn.log
sudo chown root:adm /var/log/clone_git2svn.log
sudo cp $SCRIPTS/cron/etc/clone_git2svn-log /etc/logrotate.d/clone_git2svn
sudo chmod 644 /etc/logrotate.d/clone_git2svn
sudo chown root:root /etc/logrotate.d/clone_git2svn
# (root) /etc/cron.hourly/clone_git2svn

# Rsync Backup Job
sudo cp $SCRIPTS/rsync_backup.sh /root/bin/rsync_backup.sh
#sudo cp $SCRIPTS/rsync_backup2.sh /root/bin/rsync_backup.sh
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

# Auto PowerOff Job
sudo cp $SCRIPTS/cron/bin/auto-poweroff /root/bin/auto-poweroff
sudo vim /root/bin/auto-poweroff
sudo chmod 700 /root/bin/auto-poweroff
sudo chown -R root:root /root/bin
sudo touch /var/log/auto-poweroff
sudo chmod 640 /var/log/auto-poweroff
sudo chown root:adm /var/log/auto-poweroff

# Edit crontab
sudo vim /etc/crontab $SCRIPTS/cron/plagger/crontab


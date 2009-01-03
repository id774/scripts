#!/bin/sh
#
########################################################################
# cronJob Setup Script 3
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.4 11/27,2008
#       Increase plagger scripts.
#  v1.3 9/11,2008
#       Split.
########################################################################

# Plagger
sudo useradd -m plagger
sudo passwd plagger
sudo chsh -s /bin/false plagger
sudo mkdir -p /home/plagger/.plagger_tmp/tmp1
sudo mkdir -p /home/plagger/.plagger_tmp/tmp2
sudo mkdir -p /home/plagger/.plagger_tmp/tmp3
sudo mkdir -p /home/plagger/.plagger_tmp/tmp4
sudo mkdir -p /home/plagger/.plagger_tmp/tmp5
sudo mkdir -p /home/plagger/.plagger_tmp/tmp6
sudo mkdir -p /home/plagger/.plagger_tmp/2ch
sudo mkdir -p /home/plagger/bin
sudo mkdir -p /home/plagger/yaml
sudo chmod -R 750 /home/plagger
sudo cp $SCRIPTS/cron/plagger/bin/* /home/plagger/bin/
sudo vim /home/plagger/bin/*
sudo chmod 750 /home/plagger/bin/*
sudo cp $SCRIPTS/cron/plagger/yaml/* /home/plagger/yaml/
sudo vim /home/plagger/yaml/*
sudo chmod 640 /home/plagger/yaml/*
sudo cp $SCRIPTS/cron/etc/plagger-log /etc/logrotate.d/plagger
sudo chmod 644 /etc/logrotate.d/plagger
sudo chown root:root /etc/logrotate.d/plagger
sudo touch /var/log/plagger.log
sudo touch /var/log/plaggersbm.log
sudo touch /var/log/plagger2ch2twitter.log
sudo touch /var/log/plagger2ch2gmail.log
sudo chmod 640 /var/log/plagger.log
sudo chmod 640 /var/log/plaggersbm.log
sudo chmod 640 /var/log/plagger2ch2twitter.log
sudo chmod 640 /var/log/plagger2ch2gmail.log
sudo chown plagger:adm /var/log/plagger.log
sudo chown plagger:adm /var/log/plaggersbm.log
sudo chown plagger:adm /var/log/plagger2ch2twitter.log
sudo chown plagger:adm /var/log/plagger2ch2gmail.log
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

# Edit crontab
sudo vim /etc/crontab $SCRIPTS/cron/plagger/crontab


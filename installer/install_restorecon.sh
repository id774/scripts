#!/bin/sh
#
########################################################################
# Setup restorecon cron job
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2014-06-02
#       First.
########################################################################

# Make Directory
test -d /etc/cron.weekday || sudo mkdir /etc/cron.weekday
test -d /etc/cron.weekend || sudo mkdir /etc/cron.weekend
test -d /var/log/sysadmin || sudo mkdir /var/log/sysadmin
sudo chmod 750 /var/log/sysadmin
sudo chown root:adm /var/log/sysadmin

# Setup Scripts
sudo cp $SCRIPTS/cron/bin/restorecon.sh /root/bin/restorecon.sh
sudo chmod 700 /root/bin/restorecon.sh
sudo chown root:root /root/bin/restorecon.sh

sudo cp $SCRIPTS/cron/bin/restorecon /etc/cron.weekly/restorecon
sudo vi /etc/cron.weekly/restorecon
sudo chmod 750 /etc/cron.weekly/restorecon
sudo chown root:adm /etc/cron.weekly/restorecon

sudo touch /var/log/sysadmin/restorecon.log
sudo chmod 640 /var/log/sysadmin/restorecon.log
sudo chown root:adm /var/log/sysadmin/restorecon.log

sudo cp $SCRIPTS/cron/etc/logrotate.d/restorecon /etc/logrotate.d/restorecon
sudo chmod 644 /etc/logrotate.d/restorecon
sudo chown root:root /etc/logrotate.d/restorecon

# Edit crontab
# 50 23 * * 1-5 root cd / && run-parts --report /etc/cron.weekday
# 55 6  * * 6   root cd / && run-parts --report /etc/cron.weekend
sudo vi /etc/crontab $SCRIPTS/installer/install_clamscan.sh


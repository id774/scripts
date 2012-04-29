#!/bin/sh
#
########################################################################
# cronJob Setup Script 3
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.4 6/15,2011
#       Split.
########################################################################

# Make Directory
test -d /etc/cron.weekday || sudo mkdir /etc/cron.weekday
test -d /etc/cron.weekend || sudo mkdir /etc/cron.weekend
test -d /var/log/sysadmin || sudo mkdir /var/log/sysadmin
sudo chmod 750 /var/log/sysadmin
sudo chown root:adm /var/log/sysadmin

# ClamAV AutoScan
sudo cp $SCRIPTS/cron/bin/clamscan.sh /root/bin/clamscan.sh
sudo chmod 700 /root/bin/clamscan.sh
sudo chown root:root /root/bin/clamscan.sh
sudo cp $SCRIPTS/cron/etc/clamscan_exclude /root/bin/clamscan_exclude
sudo vi /root/bin/clamscan_exclude
sudo chmod 600 /root/bin/clamscan_exclude
sudo chown root:root /root/bin/clamscan_exclude
sudo cp $SCRIPTS/cron/bin/clamscan /etc/cron.weekend/clamscan
sudo vi /etc/cron.weekend/clamscan
sudo chmod 750 /etc/cron.weekend/clamscan
sudo chown root:adm /etc/cron.weekend/clamscan
sudo touch /var/log/clamav/clamscan.log
sudo chmod 640 /var/log/clamav/clamscan.log
sudo chown clamav:adm /var/log/clamav/clamscan.log
sudo touch /var/log/clamav/clamav.log
sudo chmod 640 /var/log/clamav/clamav.log
sudo chown clamav:adm /var/log/clamav/clamav.log
sudo cp $SCRIPTS/cron/etc/logrotate.d/clamscan /etc/logrotate.d/clamscan
sudo chmod 644 /etc/logrotate.d/clamscan
sudo chown root:root /etc/logrotate.d/clamscan

# Archive Old Logs
if [ -f /var/log/clamav_upgrade.log ]; then
    test -d /var/log/sysadmin/archive || sudo mkdir /var/log/sysadmin/archive
    sudo chmod 750 /var/log/sysadmin/archive
    sudo chown root:adm /var/log/sysadmin/archive
    test -f /var/log/clamav_upgrade.log* || sudo mv /var/log/clamav_upgrade.log* /var/log/sysadmin/archive/
fi

if [ -f /var/log/clamscan.log ]; then
    test -d /var/log/sysadmin/archive || sudo mkdir /var/log/sysadmin/archive
    sudo chmod 750 /var/log/sysadmin/archive
    sudo chown root:adm /var/log/sysadmin/archive
    test -f /var/log/clamscan.log* || sudo mv /var/log/clamscan.log* /var/log/sysadmin/archive/
fi

if [ -f /var/log/clamav.log ]; then
    test -d /var/log/sysadmin/archive || sudo mkdir /var/log/sysadmin/archive
    sudo chmod 750 /var/log/sysadmin/archive
    sudo chown root:adm /var/log/sysadmin/archive
    test -f /var/log/clamav.log* && sudo mv /var/log/clamav.log* /var/log/sysadmin/archive/
fi

# Edit crontab
# 50 23 * * 1-5 root cd / && run-parts --report /etc/cron.weekday
# 55 6  * * 6   root cd / && run-parts --report /etc/cron.weekend
sudo vi /etc/crontab $SCRIPTS/installer/install_clamscan.sh


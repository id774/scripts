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

# ClamAV AutoScan
sudo cp $SCRIPTS/cron/bin/clamscan.sh /root/bin/clamscan.sh
sudo chmod 700 /root/bin/clamscan.sh
sudo chown root:root /root/bin/clamscan.sh
sudo cp $SCRIPTS/cron/etc/clamscan_exclude /root/bin/clamscan_exclude
sudo vim /root/bin/clamscan_exclude
sudo chmod 600 /root/bin/clamscan_exclude
sudo chown root:root /root/bin/clamscan_exclude
test -d /etc/cron.weekend || sudo mkdir /etc/cron.weekend
sudo cp $SCRIPTS/cron/bin/clamscan /etc/cron.weekend/clamscan
sudo vim /etc/cron.weekend/clamscan
sudo chmod 750 /etc/cron.weekend/clamscan
sudo chown root:adm /etc/cron.weekend/clamscan
sudo touch /var/log/clamav/clamscan.log
sudo chmod 640 /var/log/clamav/clamscan.log
sudo chown clamav:adm /var/log/clamav/clamscan.log
sudo touch /var/log/clamav/clamav.log
sudo chmod 640 /var/log/clamav/clamav.log
sudo chown clamav:adm /var/log/clamav/clamav.log
sudo cp $SCRIPTS/cron/etc/clamscan-log /etc/logrotate.d/clamscan
sudo chmod 644 /etc/logrotate.d/clamscan
sudo chown root:root /etc/logrotate.d/clamscan

# Edit crontab
# 50 23 * * 1-5 root cd / && run-parts --report /etc/cron.weekday
# 55 6  * * 6   root cd / && run-parts --report /etc/cron.weekend
sudo vim /etc/crontab


#!/bin/sh
#
########################################################################
# Apache Summary Report Setup Script
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 2023-12-16
#       Also install ignore list.
#  v1.0 2022-10-11
#       Stable.
########################################################################

# Make Directory
test -d /var/log/sysadmin || sudo mkdir /var/log/sysadmin
sudo chmod 750 /var/log/sysadmin
sudo chown root:adm /var/log/sysadmin

# Server Resource Report Job
sudo cp $SCRIPTS/apache_log_analysis.sh /root/bin/apache_log_analysis.sh
sudo chmod 700 /root/bin/apache_log_analysis.sh
sudo chown root:root /root/bin/apache_log_analysis.sh
sudo cp $SCRIPTS/etc/apache_ignore.list /root/etc/apache_ignore.list
sudo chmod 600 /root/etc/apache_ignore.list
sudo chown root:root /root/etc/apache_ignore.list
sudo cp $SCRIPTS/cron/bin/apache_log_analysis /etc/cron.daily/apache_log_analysis
sudo chmod 750 /etc/cron.daily/apache_log_analysis
sudo chown root:adm /etc/cron.daily/apache_log_analysis
sudo touch /var/log/sysadmin/apache_summary.log
sudo chmod 640 /var/log/sysadmin/apache_summary.log
sudo chown root:adm /var/log/sysadmin/apache_summary.log
sudo cp $SCRIPTS/cron/etc/logrotate.d/apache_summary /etc/logrotate.d/apache_summary
sudo chmod 644 /etc/logrotate.d/apache_summary
sudo chown root:root /etc/logrotate.d/apache_summary


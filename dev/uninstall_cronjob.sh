#!/bin/sh
#
########################################################################
# Uninstall obsolete cron job
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 10/24,2011
#       First.
########################################################################

remove_cronjob() {
    test -f /etc/cron.hourly/$1 && \
      sudo rm -v /etc/cron.hourly/$1
    test -f /etc/cron.daily/$1 && \
      sudo rm -v /etc/cron.daily/$1
    test -f /etc/cron.weekly/$1 && \
      sudo rm -v /etc/cron.weekly/$1
    test -f /etc/cron.weekday/$1 && \
      sudo rm -v /etc/cron.weekday/$1
    test -f /etc/cron.weekend/$1 && \
      sudo rm -v /etc/cron.weekend/$1
    test -f /etc/cron.monthly/$1 && \
      sudo rm -v /etc/cron.monthly/$1
}

remove_obsolete_jobs() {
    edit_cronjob auto-upgrade
    edit_cronjob backup
}

remove_logs() {
    sudo rm -f /var/log/sysadmin/backup.log*
    sudo rm -v /etc/logrotate.d/backup
    sudo rm -f /var/log/sysadmin/auto-upgrade.log*
    sudo rm -v /etc/logrotate.d/auto-upgrade
}

sudo rm -v /root/bin/backup.sh
sudo rm -v /root/bin/backup_exclude
remove_logs
remove_obsolete_jobs

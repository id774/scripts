#!/bin/sh

test -f /usr/local/sbin/clamd            && sudo rm -f /usr/local/sbin/clamd
test -f /usr/local/bin/clamav-config     && sudo rm -f /usr/local/bin/clamav-config
test -f /usr/local/bin/clambc            && sudo rm -f /usr/local/bin/clambc
test -f /usr/local/bin/clamconf          && sudo rm -f /usr/local/bin/clamconf
test -f /usr/local/bin/clamdscan         && sudo rm -f /usr/local/bin/clamdscan
test -f /usr/local/bin/clamdtop          && sudo rm -f /usr/local/bin/clamdtop
test -f /usr/local/bin/clamscan          && sudo rm -f /usr/local/bin/clamscan
test -f /usr/local/bin/freshclam         && sudo rm -f /usr/local/bin/freshclam
test -f /usr/local/bin/sigtool           && sudo rm -f /usr/local/bin/sigtool
test -f /usr/local/lib/libclamav.so      && sudo rm -f /usr/local/lib/libclam*
test -d /usr/local/share/clamav          && sudo rm -rf /usr/local/share/clamav
test -f /usr/local/etc/clamd.conf        && sudo rm -f /usr/local/etc/clamd.conf*
test -f /usr/local/etc/freshclam.conf    && sudo rm -f /usr/local/etc/freshclam.conf*
test -f /etc/cron.weekly/clamav_upgrade  && sudo rm -f /etc/cron.weekly/clamav_upgrade
test -f /etc/cron.monthly/clamav_upgrade && sudo rm -f /etc/cron.monthly/clamav_upgrade
test -f /etc/logrotate.d/clamav_upgrade  && sudo rm -f /etc/logrotate.d/clamav_upgrade
test -f /var/log/clamscan.log            && sudo rm -f /var/log/clamscan.log*
test -f /var/log/clamav_upgrade          && sudo rm -f /var/log/clamav_upgrade*
test -d /usr/local/src/security          && sudo rm -rf /usr/local/src/security

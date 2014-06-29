#!/bin/sh
#
########################################################################
# chkrootkit setup script
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 5/15,2012
#       First.
########################################################################

# job
sudo cp $SCRIPTS/cron/bin/chkrootkit /etc/cron.weekly/chkrootkit
sudo chmod 750 /etc/cron.weekly/chkrootkit
sudo chown root:adm /etc/cron.weekly/chkrootkit
sudo vi /etc/cron.weekly/chkrootkit
# logdir
test -d /var/log/chkrootkit || sudo mkdir /var/log/chkrootkit
sudo chmod 750 /var/log/chkrootkit
sudo chown root:adm /var/log/chkrootkit
# log
sudo touch /var/log/chkrootkit/chkrootkit.log
sudo chmod 640 /var/log/chkrootkit/chkrootkit.log
sudo chown root:adm /var/log/chkrootkit/chkrootkit.log
# logrotate
sudo cp $SCRIPTS/cron/etc/logrotate.d/chkrootkit /etc/logrotate.d/chkrootkit
sudo chmod 644 /etc/logrotate.d/chkrootkit
sudo chown root:root /etc/logrotate.d/chkrootkit

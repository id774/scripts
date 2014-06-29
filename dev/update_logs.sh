#!/bin/sh

test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts

update_log() {
    if [ -f $2 ]; then
        sudo cp $SCRIPTS/cron/bin/$1 $2
        sudo chown root:adm $2
        sudo chmod 750 $2
        sudo vim $2
    fi
}

if [ -f /opt/deferred-sync/bin/run ]; then
    sudo cp $HOME/deferred-sync/bin/run /opt/deferred-sync/bin/run
    sudo chown root:root /opt/deferred-sync/bin/run
    sudo chmod 755 /opt/deferred-sync/bin/run
fi

if [ -f /etc/rc.local.d/mail_to_admin ]; then
    sudo cp $SCRIPTS/etc/rc.local.d/mail_to_admin /etc/rc.local.d/mail_to_admin
    sudo chown root:adm /etc/rc.local.d/mail_to_admin
    sudo chmod 740 /etc/rc.local.d/mail_to_admin
    sudo vim /etc/rc.local.d/mail_to_admin
fi

test -f /etc/cron.daily/chkrootkit && sudo rm -f /etc/cron.daily/chkrootkit
update_log clamscan /etc/cron.weekend/clamscan
update_log chkrootkit /etc/cron.weekly/chkrootkit
update_log rsync_backup /etc/cron.hourly/rsync_backup
update_log get_resources /etc/cron.hourly/get_resources
update_log restorecon /etc/cron.weekly/restorecon


#!/bin/sh

sudo cp $SCRIPTS/cron/bin/smartctl_monitor.sh /root/bin/
sudo chown root:root /root/bin/smartctl_monitor.sh
sudo chmod 700 /root/bin/smartctl_monitor.sh
sudo cp $SCRIPTS/cron/etc/cron.d/smartctl-monitor /etc/cron.d/
sudo chown root:root /etc/cron.d/smartctl-monitor
sudo chmod 644 /etc/cron.d/smartctl-monitor


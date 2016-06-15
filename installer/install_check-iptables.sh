#!/bin/sh

sudo cp $SCRIPTS/cron/bin/check-iptables /etc/cron.hourly/check-iptables
sudo chmod 750 /etc/cron.hourly/check-iptables
sudo chown root:adm /etc/cron.hourly/check-iptables
sudo vim /etc/cron.hourly/check-iptables

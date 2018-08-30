#!/bin/sh

sudo cp $SCRIPTS/cron/bin/check-ntpd /etc/cron.hourly/check-ntpd
sudo chmod 750 /etc/cron.hourly/check-ntpd
sudo chown root:adm /etc/cron.hourly/check-ntpd
sudo vim /etc/cron.hourly/check-ntpd

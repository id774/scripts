#!/bin/sh

sudo cp $SCRIPTS/cron/bin/check-termtter /etc/cron.hourly/check-termtter
sudo chmod 750 /etc/cron.hourly/check-termtter
sudo chown root:adm /etc/cron.hourly/check-termtter
sudo vim /etc/cron.hourly/check-termtter

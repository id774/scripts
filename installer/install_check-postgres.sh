#!/bin/sh

sudo cp $SCRIPTS/cron/bin/check-postgres /etc/cron.hourly/check-postgres
sudo chmod 750 /etc/cron.hourly/check-postgres
sudo chown root:adm /etc/cron.hourly/check-postgres
sudo vim /etc/cron.hourly/check-postgres

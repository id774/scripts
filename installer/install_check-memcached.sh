#!/bin/sh

sudo cp $SCRIPTS/cron/bin/check-memcached /etc/cron.hourly/check-memcached
sudo chmod 750 /etc/cron.hourly/check-memcached
sudo chown root:adm /etc/cron.hourly/check-memcached
sudo vim /etc/cron.hourly/check-memcached

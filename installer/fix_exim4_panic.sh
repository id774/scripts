#!/bin/sh

sudo vim /etc/exim4/update-exim4.conf.conf
# dc_local_interfaces='127.0.0.1 ; ::1' -> dc_local_interfaces='127.0.0.1'

sudo rm /var/log/exim4/paniclog

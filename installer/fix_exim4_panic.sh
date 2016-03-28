#!/bin/sh

sudo sed -i "s/dc_local_interfaces='127.0.0.1 ; ::1'/dc_local_interfaces='127.0.0.1'/" /etc/exim4/update-exim4.conf.conf

test -f /var/log/exim4/paniclog && sudo rm /var/log/exim4/paniclog

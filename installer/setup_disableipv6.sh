#!/bin/sh

sudo cp ~/scripts/etc/sysctl.d/disableipv6.conf /etc/sysctl.d/
sudo chown root:root /etc/sysctl.d/disableipv6.conf
sudo sysctl -p /etc/sysctl.d/disableipv6.conf

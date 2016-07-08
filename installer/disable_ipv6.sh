#!/bin/sh

sudo sh -c 'echo "net.ipv6.conf.all.disable_ipv6 = 1">>/etc/sysctl.conf'
sudo sh -c 'echo "net.ipv6.conf.default.disable_ipv6 = 1">>/etc/sysctl.conf'
sudo sh -c 'echo "net.ipv6.conf.lo.disable_ipv6 = 1">>/etc/sysctl.conf'

sudo vim /etc/sysctl.conf

sudo sysctl -p

ip a | grep inet6

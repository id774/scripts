#!/bin/sh

# To do:
# * Remove the line that begins with SUBSYSTEM=="net" from /etc/udev/rules.d/70-persistent-net.rules 
# * Delete rows following 2 lines in /etc/udev/rules.d/75-persistent-net-generator.rules
#   - ACTION=="add", SUBSYSTEM=="net", KERNEL=="eth*|ath*|wlan*|ra*|sta*" \
#   - NAME!="?*", DRIVERS=="?*", GOTO="persistent_net_generator_do"

sudo vim /etc/udev/rules.d/*

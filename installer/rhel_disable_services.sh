#!/bin/sh

systemctl disable auditd.service
systemctl disable abrt-ccpp.service
systemctl disable abrt-oops.service
systemctl disable abrt-xorg.service
systemctl disable abrt-vmcore.service
systemctl disable abrtd.service
systemctl disable libstoragemgmt.service
systemctl disable cups.service
systemctl disable iscsi.service
systemctl disable bluetooth.service
systemctl disable ModemManager.service
systemctl disable libvirtd.service
systemctl disable qemu-guest-agent.service
systemctl disable multipathd.service
systemctl disable dmraid-activation.service
systemctl disable mdmonitor.service
systemctl disable avahi-daemon.service

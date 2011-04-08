#!/bin/sh
#
########################################################################
# Solaris Batch Network Configuration
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 4/8,2011
#       First.
########################################################################

network_configuration() {
    # hosts
    pfexec vim /etc/inet/hosts

    # nodename
    pfexec vim /etc/nodename

    # hostname.eri0
    pfexec vim /etc/hostname.eri0

    # netmask
    pfexec vim /etc/netmasks

    # default router
    pfexec vim /etc/defaultrouter

    # nsswitch
    pfexec vim /etc/nsswitch.conf

    # dns
    pfexec vim /etc/resolv.conf

    # service restart
    pfexec svcadm restart svc:/network/physical:default
}

# Solaris?
case `uname -s` in
  *SunOS*)
    network_configuration
    ;;
  *)
    exit 1
    ;;
esac

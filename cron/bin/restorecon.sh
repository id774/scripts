#!/bin/sh
#
########################################################################
# SELinux Restore Context
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2014-06-02
#       First.
########################################################################

restorecon -RFv /usr
restorecon -RFv /opt
restorecon -RFv /etc
restorecon -RFv /var
restorecon -RFv /root
restorecon -RFv /home


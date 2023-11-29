#!/bin/sh
#
########################################################################
# User List Display Script (userlist.sh)
#
#  Description:
#  This script lists user accounts with UIDs greater than a specified threshold.
#  The threshold is set based on the existence of system-specific files,
#  distinguishing between Debian-based and RedHat-based systems.
#
#  Author: id774
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 11/16,2014
#       Initial release. Detects system type and displays user accounts with
#       UIDs above the system-specific threshold.
#
# Usage:
#  Run the script without any arguments:
#      userlist.sh
#
#  The script automatically detects the system type and sets the UID threshold
#  accordingly. It then displays a list of user accounts with UIDs above this threshold.
#
########################################################################

THRESHOLD=0
test -f /etc/debian_version && THRESHOLD=1000
test -f /etc/redhat-release && THRESHOLD=500

show_userlist() {
  while read LINE
  do
    if [ `echo $LINE | cut -f3 -d:` -ge $THRESHOLD ]
    then
      echo $LINE | cut -f1 -d:
    fi
  done < /etc/passwd
}

test $THRESHOLD = 0 || show_userlist

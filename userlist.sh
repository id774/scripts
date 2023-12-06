#!/bin/sh

########################################################################
# User List Display Script (userlist.sh)
#
#  Description:
#  This script lists user accounts with UIDs greater than a specified threshold.
#  The threshold is set based on the existence of system-specific files,
#  distinguishing between Debian-based, RedHat-based, and macOS systems.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-11-29
#       Improved script to support macOS by using 'dscacheutil' for user information retrieval.
#       Maintained compatibility with Linux and other Unix-like systems.
#  v1.0 2014-11-16
#       Initial release. Detects system type and displays user accounts with
#       UIDs above the system-specific threshold.
#
#  Usage:
#  Run the script without any arguments:
#      ./userlist.sh
#
#  The script automatically detects the system type and sets the UID threshold
#  accordingly. It then displays a list of user accounts with UIDs above this threshold.
#
########################################################################

THRESHOLD=0
test -f /etc/debian_version && THRESHOLD=1000
test -f /etc/redhat-release && THRESHOLD=500
test "$(uname)" = "Darwin" && THRESHOLD=500

show_userlist() {
  if [ "$(uname)" = "Darwin" ]; then
    # macOS
    dscacheutil -q user | awk -v thresh=$THRESHOLD '
      /uid:/ { uid=$2 }
      /name:/ { name=$2 }
      /uid:/ && uid >= thresh { print name }
    '
  else
    # Linux/Unix
    while read LINE
    do
      USER_ID=$(echo $LINE | cut -d: -f3)
      if [ -z "$USER_ID" ] || ! [ "$USER_ID" -eq "$USER_ID" ] 2> /dev/null; then
        continue
      fi
      if [ $USER_ID -ge $THRESHOLD ]
      then
        echo $LINE | cut -d: -f1
      fi
    done < /etc/passwd
  fi
}

test $THRESHOLD = 0 || show_userlist


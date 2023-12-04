#!/bin/sh
#
########################################################################
# securetty.sh: Script to Clear /etc/securetty for Unrestricted Root Access
#
#  Description:
#  This script checks if /etc/securetty is a regular file and clears its content
#  to enable root login from any TTY device. This might be necessary for certain
#  system administration tasks or policy changes.
#
#  Warning:
#  Using this script can decrease system security by allowing root access from any
#  terminal. It should only be used when absolutely necessary and in secure environments.
#  This script does nothing if /etc/securetty is a directory, which might be the case
#  in some systems like macOS.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 11/30,2023
#       Added check to ensure /etc/securetty is a file before clearing it.
#  v1.0 5/21,2012
#       Initial release. Script to clear /etc/securetty for unrestricted root access.
#
#  Usage:
#  Run the script without any arguments:
#      ./securetty.sh
#
########################################################################

if [ -f /etc/securetty ]; then
    sudo sh -c ": > /etc/securetty"
elif [ -d /etc/securetty ]; then
    echo "/etc/securetty is a directory, no changes were made."
else
    echo "/etc/securetty does not exist as a file or directory."
fi

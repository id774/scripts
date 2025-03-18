#!/bin/sh

########################################################################
# fix-permissions.sh: Fix File and Directory Permissions
#
#  Description:
#  This script adjusts the permissions and ownership of critical system
#  directories and files to ensure they are set correctly. It also logs
#  all operations and sends the log file to the system administrator via
#  email.
#
#  Key Features:
#  - Recursively changes ownership of specific directories to root.
#  - Sets appropriate permissions for cron directories and files.
#  - Logs all operations with timestamps and host details.
#  - Sends a summary of operations to a specified admin email.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-19
#       Improved POSIX compliance, standardized redirections, and enhanced readability.
#  v1.0 2024-12-09
#       Initial release with logging, permission adjustments, and email reporting.
#
#  Usage:
#  Execute the script directly without any arguments:
#      ./fix-permissions.sh
#
#  Requirements:
#  - Must be executed with sufficient permissions to change ownership
#    and modify file attributes (typically as root).
#  - The `mail` command must be installed and configured to send emails.
#  - Ensure the `nkf` command is installed for UTF-8 email conversion.
#
#  Note:
#  - The `ADMIN_MAIL_ADDRESS` environment variable can be set to specify
#    a recipient email address. Default is `root`.
#  - Logs are written to `/var/log/sysadmin/fix-permissions.log`.
#
########################################################################


LC_CTYPE=ja_JP.UTF-8
JOBLOG="/var/log/sysadmin/fix-permissions.log"

echo -n "*** $0: Job started on $(hostname) at " >> "$JOBLOG" 2>&1
date "+%Y/%m/%d %T" >> "$JOBLOG" 2>&1

echo "Setting permission for /opt/python, /opt/ruby" >> "$JOBLOG" 2>&1
chown -R root:root /opt/python
chown -R root:root /opt/ruby
echo "Setting permission for /usr/local/etc, /usr/local/src" >> "$JOBLOG" 2>&1
chown -R root:root /usr/local/etc
chown -R root:root /usr/local/src

echo "Setting permission for /etc/cron.*" >> "$JOBLOG" 2>&1
chmod -R 744 /etc/cron.hourly/*
chmod -R 744 /etc/cron.daily/*
chmod -R 744 /etc/cron.weekly/*
chmod -R 744 /etc/cron.monthly/*
chmod -R 644 /etc/cron.d/*

echo -n "*** $0: Job ended on $(hostname) at " >> "$JOBLOG" 2>&1
date "+%Y/%m/%d %T" >> "$JOBLOG" 2>&1
echo >> "$JOBLOG" 2>&1

ADMIN_MAIL_ADDRESS="root"
case "$ADMIN_MAIL_ADDRESS" in
  *@*)
    [ -r "$JOBLOG" ] && cat -v "$JOBLOG" | nkf -w | mail -s "[cron][$(hostname)] Fixed Permissions Log" "$ADMIN_MAIL_ADDRESS"
    ;;
esac

exit 0

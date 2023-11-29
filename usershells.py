#!/usr/bin/env python
#
########################################################################
# User Shell Listing Script (usershells.py)
#
#  Description:
#  This script lists user accounts and their corresponding shells from the /etc/passwd file,
#  excluding accounts with non-interactive shells like 'false', 'nologin', 'sync', 'shutdown',
#  and 'halt'.
#
#  Author: id774
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 8/30,2018
#       Improved shell filtering to exclude non-interactive system accounts such as 'sync',
#       'shutdown', and 'halt'.
#  v1.0 2/14,2017
#       Initial release. Lists user accounts and their shells, excluding 'false' and 'nologin'.
#
# Usage:
#  Run the script without any arguments:
#      python usershells.py
#
#  The script parses /etc/passwd and outputs a list of user accounts with interactive shells.
#
########################################################################

passwd = "/etc/passwd"
fo = open(passwd, 'r')

shells = {}
for line in fo:
    line = line.strip()
    fields = line.split(":")
    shells[fields[0]] = fields[-1]

fo.close()

for account in shells.keys():
    if 'false' not in shells[account] and 'nologin' not in shells[account] and 'sync' not in shells[account] and 'shutdown' not in shells[account] and 'halt' not in shells[account]:
        print("{0:11} => {1}".format(account, shells[account]))

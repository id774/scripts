#!/usr/bin/env python

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

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
    if not (shells[account].endswith('false') or shells[account].endswith('nologin')):
        print("{0:11} => {1}".format(account, shells[account]))

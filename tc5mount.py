#!/usr/bin/env python

import sys, os

def os_exec(cmd0, cmd1, device):
    os.system('dmesg | grep ' + device)
    os.system(cmd1)
    os.system(cmd0)

def os_command(mount_options, device):
    cmd1 = 'test -b /dev/' + device + '1 && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=' + mount_options + ' /dev/' + device + '1 ~/mnt/' + device 
    cmd0 = 'test -b /dev/' + device + '1 || ( test -b /dev/' + device + ' && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=' + mount_options + ' /dev/' + device + ' ~/mnt/' + device + ' )'
    os_exec(cmd0, cmd1, device)

def mount_local(options, args):
    cmd = 'test -f ~/local/`/bin/hostname`.tc && test -d ~/mnt/tc && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=utf8 ~/local/`/bin/hostname`.tc ~/mnt/tc'
    os.system(cmd)
    if options.local:
        pass
    else:
        mount_device(options, args)

def mount_device(options, args):
    mount_options = 'utf8'
    if options.readonly:
        mount_options = 'ro,' + mount_options

    os_command(mount_options, 'sdb')
    if options.all:
        mount_all(options, args, mount_options)

def mount_all(options, args, mount_options):
    if options.half and not options.readonly:
        mount_options = 'ro,' + mount_options

    os_command(mount_options, 'sdc')
    os_command(mount_options, 'sdd')
    os_command(mount_options, 'sde')
    os_command(mount_options, 'sdf')

def main():
    from optparse import OptionParser
    usage = "usage: %prog [options]"
    parser = OptionParser(usage)
    parser.add_option("-r", "--readonly",
                      dest="readonly",
                      help="read only",
                      action="store_true")
    parser.add_option("-a", "--all",
                      dest="all",
                      help="mount all devices",
                      action="store_true")
    parser.add_option("-l", "--local",
                      dest="local",
                      help="mount local file only",
                      action="store_true")
    parser.add_option("-f", "--half",
                      dest="half",
                      help="mount half readwrite and readonly",
                      action="store_true")
    (options, args) = parser.parse_args()
    if len(args) == 0:
        mount_local(options, args)
    else:
        parser.print_help()

if __name__ == "__main__":
    main()


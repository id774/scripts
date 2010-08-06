#!/usr/bin/env python
#
########################################################################
# TrueCrypt Device Mounter
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/6,2010
#       Stable.
########################################################################

import sys, os

def os_exec(cmd, device):
    os.system('dmesg | grep ' + device)
    os.system(cmd)

def os_command(options, args, mount_options, device):
    if options.partition:
        cmd = 'test -b /dev/' + device + options.partition +\
        ' && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=' +\
        mount_options + ' /dev/' + device + options.partition + ' ~/mnt/' + device 
    else:
        cmd = 'test -b /dev/' + device +\
        ' && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=' +\
        mount_options + ' /dev/' + device + ' ~/mnt/' + device
    os_exec(cmd, device)

def mount_local(options, args):
    cmd = 'test -f ~/local/`/bin/hostname`.tc &&\
           test -d ~/mnt/tc && sudo truecrypt -t -k "" --protect-hidden=no\
           --fs-options=utf8 ~/local/`/bin/hostname`.tc ~/mnt/tc'
    os.system(cmd)
    if options.local:
        pass
    else:
        mount_device(options, args)

def mount_device(options, args):
    mount_options = 'utf8'
    if options.readonly:
        mount_options = 'ro,' + mount_options

    os_command(options, args, mount_options, 'sdb')
    if options.all:
        mount_all(options, args, mount_options)

def mount_all(options, args, mount_options):
    if options.half and not options.readonly:
        mount_options = 'ro,' + mount_options

    os_command(options, args, mount_options, 'sdc')
    os_command(options, args, mount_options, 'sdd')
    os_command(options, args, mount_options, 'sde')
    os_command(options, args, mount_options, 'sdf')

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
    parser.add_option("-p", "--partition", dest="partition",
                      help="partition number")
    (options, args) = parser.parse_args()
    if len(args) == 0:
        mount_local(options, args)
    else:
        parser.print_help()

if __name__ == "__main__":
    main()


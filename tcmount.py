#!/usr/bin/env python
#
########################################################################
# TrueCrypt Device Mounter
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v2.1 9/26,2016
#       Correspond to sdz.
#  v2.0 5/26,2014
#       Show version on console.
#       Mount with crypt files on home dir.
#  v1.3 5/8,2014
#       Specify -u option for mounting with utf8, default is none.
#  v1.2 4/14,2013
#       Implement file mount function.
#  v1.1 1/26,2012
#       Refactoring, and for legacy device.
#  v1.0 8/6,2010
#       Stable.
########################################################################

import sys
import os

def os_exec(cmd, device):
    os.system('dmesg | grep ' + device)
    os.system(cmd)

def mount_drive(options, args, mount_options, device):
    if options.partition:
        cmd = 'test -b /dev/' + device + options.partition +\
            ' && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=' +\
            mount_options + ' /dev/' + device + \
            options.partition + ' ~/mnt/' + device
    else:
        cmd = 'test -b /dev/' + device +\
            ' && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=' +\
            mount_options + ' /dev/' + device + ' ~/mnt/' + device
    os_exec(cmd, device)

def mount_file(options, args, mount_options, device, mount_point, filename):
    cmd = 'test -f /mnt/' + device + '/' + filename +\
        ' && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=' +\
        mount_options + ' /mnt/' + device + '/' + \
        filename + ' ~/mnt/' + mount_point
    os_exec(cmd, device)

def partition(options, args, mount_options, device):
    mount_drive(options, args, mount_options, device)
    mount_file(options, args, mount_options, device, device + '1', 'data1')
    mount_file(options, args, mount_options, device, device + '2', 'data2')

def mount_device(options, args):
    mount_options = ''
    if options.utf8:
        mount_options = 'utf8'
    if options.readonly:
        mount_options = ",".join(('ro', mount_options))

    partition(options, args, mount_options, 'sdb')

    if options.all:
        mount_all(options, args, mount_options)

def mount_all(options, args, mount_options):
    if options.half and not options.readonly:
        mount_options = ",".join(('ro', mount_options))

    partition(options, args, mount_options, 'sdc')
    partition(options, args, mount_options, 'sdd')
    partition(options, args, mount_options, 'sde')
    partition(options, args, mount_options, 'sdf')
    partition(options, args, mount_options, 'sdg')
    partition(options, args, mount_options, 'sdh')
    partition(options, args, mount_options, 'sdi')
    partition(options, args, mount_options, 'sdj')
    partition(options, args, mount_options, 'sdk')
    partition(options, args, mount_options, 'sdl')
    partition(options, args, mount_options, 'sdm')
    partition(options, args, mount_options, 'sdn')
    partition(options, args, mount_options, 'sdo')
    partition(options, args, mount_options, 'sdp')
    partition(options, args, mount_options, 'sdq')
    partition(options, args, mount_options, 'sdr')
    partition(options, args, mount_options, 'sds')
    partition(options, args, mount_options, 'sdt')
    partition(options, args, mount_options, 'sdu')
    partition(options, args, mount_options, 'sdv')
    partition(options, args, mount_options, 'sdw')
    partition(options, args, mount_options, 'sdx')
    partition(options, args, mount_options, 'sdy')
    partition(options, args, mount_options, 'sdz')

def mount_legacy(options, args):
    mount_local('pc98a', options)
    mount_local('pc98b', options)
    mount_local('data1', options)
    mount_local('data2', options)

def mount_local(device, options):
    mount_options = ''
    if options.utf8:
        mount_options = 'utf8'
    cmd = 'test -f ~/local/' + device + '.tc && ' +\
          'test -d ~/mnt/' + device +\
          ' && sudo truecrypt -t -k "" --protect-hidden=no ' +\
          '--fs-options=' + mount_options + \
        ' ~/local/' + device + '.tc ~/mnt/' + device
    os.system(cmd)
    cmd = 'test -f /data/crypt/' + device +\
        ' && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=' +\
        mount_options + ' /data/crypt/' + device + ' ~/mnt/' + device
    os_exec(cmd, device)

def tcmount(options, args):
    mount_local('`/bin/hostname`', options)
    if options.legacy or options.all:
        mount_legacy(options, args)
    if options.local:
        pass
    else:
        mount_device(options, args)

def main():
    version = "2.1"
    from optparse import OptionParser
    usage = "usage: %prog [options]"
    parser = OptionParser(usage)
    parser.add_option("-u", "--utf8",
                      dest="utf8",
                      help="mount filesystem type with utf8",
                      action="store_true")
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
    parser.add_option("-g", "--legacy",
                      dest="legacy",
                      help="mount legacy device",
                      action="store_true")
    parser.add_option("-f", "--half",
                      dest="half",
                      help="mount half readwrite and readonly",
                      action="store_true")
    parser.add_option("-p", "--partition", dest="partition",
                      help="partition number")
    (options, args) = parser.parse_args()
    if len(args) == 0:
        print("tcmount " + version)
        tcmount(options, args)
    else:
        parser.print_help()

if __name__ == "__main__":
    main()

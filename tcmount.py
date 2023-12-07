#!/usr/bin/env python

########################################################################
# tcmount.py: TrueCrypt Device Mounter
#
#  Description:
#  This script is designed to automate the mounting of TrueCrypt
#  encrypted devices. It supports a variety of devices and includes
#  options for different file systems and encoding types.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.2 2018-08-22
#       Requires privilege to run dmesg.
#  v2.1 2016-09-26
#       Correspond to sdz.
#  v2.0 2014-05-26
#       Show version on console.
#       Mount with crypt files on home dir.
#  v1.3 2014-05-08
#       Specify -u option for mounting with utf8, default is none.
#  v1.2 2013-04-14
#       Implement file mount function.
#  v1.1 2012-01-26
#       Refactoring, and for legacy device.
#  v1.0 2010-08-06
#       Stable.
#
#  Usage:
#  To use this script, ensure you have TrueCrypt installed and run
#  the script with appropriate privileges. You can specify the device
#  and other mount options as arguments. For example:
#
#      python tcmount.py [device] [options]
#
#  Refer to the TrueCrypt documentation for more detailed information
#  on mount options and device specifications.
#
########################################################################

import os

def os_exec(cmd, device):
    os.system('sudo dmesg | grep ' + device)
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
    version = "2.2"
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

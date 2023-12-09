#!/usr/bin/env python

########################################################################
# tcmount.py: TrueCrypt Device Mounter
#
#  Description:
#  This script is designed to automate the mounting and unmounting of TrueCrypt
#  encrypted devices. It checks for the presence of the TrueCrypt command and
#  supports a variety of devices, including options for different file systems
#  and encoding types. This version allows for specific device mounting and 
#  unmounting by specifying the device name as an argument.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v3.1 2023-12-10
#       Minor refactoring of the os_exec function.
#       Enhanced version display to include TrueCrypt version.
#  v3.0 2023-12-09
#       Refactored for improved readability and maintenance.
#       Added specific device mounting and unmounting functionalities.
#  v2.3 2023-12-08
#       Added check for TrueCrypt command presence.
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
#       First release.
#
#  Usage:
#  To use this script, ensure you have TrueCrypt installed and run
#  the script with appropriate privileges. You can specify the device
#  and other mount options as arguments. For example:
#
#      python tcmount.py [device] [options]
#
#  Specific device mounting:
#      python tcmount.py sdb
#      This will mount the device /dev/sdb.
#
#  Specific device unmounting:
#      python tcmount.py sdb unmount
#      python tcmount.py sdb umount
#      These commands will unmount the device /dev/sdb.
#
#  Options:
#  -u, --utf8         Mount filesystem with UTF-8 encoding.
#  -r, --readonly     Mount the filesystem in read-only mode.
#  -a, --all          Mount all available devices.
#  -l, --local        Mount only local files.
#  -g, --legacy       Mount legacy devices.
#  -f, --half         Mount half of the devices read-write and the other half read-only.
#  -p, --partition    Specify a particular partition number to mount.
#
#  Refer to the TrueCrypt documentation for more detailed information
#  on mount options and device specifications.
#
########################################################################

import os
import sys
from optparse import OptionParser
import subprocess

def os_exec(cmd):
    """
    Executes a system command using subprocess.
    """
    subprocess.call(cmd, shell=True)

def is_truecrypt_installed():
    """
    Checks if TrueCrypt is installed by searching for its command in the system path.
    """
    return subprocess.call(['which', 'truecrypt'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL) == 0

def get_truecrypt_version():
    """
    Retrieves the version information of TrueCrypt.
    """
    try:
        output = subprocess.check_output(
            ["truecrypt", "--version"], stderr=subprocess.STDOUT)
        return output.decode().strip()
    except subprocess.CalledProcessError:
        return "Unknown"

def mount_drive(options, args, mount_options, device):
    """
    Mounts a TrueCrypt volume on a specific drive.
    """
    if options.partition:
        cmd = 'test -b /dev/' + device + options.partition +\
            ' && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=' +\
            mount_options + ' /dev/' + device + \
            options.partition + ' ~/mnt/' + device
    else:
        cmd = 'test -b /dev/' + device +\
            ' && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=' +\
            mount_options + ' /dev/' + device + ' ~/mnt/' + device
    os_exec(cmd)

def mount_file(options, args, mount_options, device, mount_point, filename):
    """
    Mounts a TrueCrypt volume from a file.
    """
    cmd = 'test -f /mnt/' + device + '/' + filename +\
        ' && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=' +\
        mount_options + ' /mnt/' + device + '/' + \
        filename + ' ~/mnt/' + mount_point
    os_exec(cmd)

def partition(options, args, mount_options, device):
    """
    Handles partition mounting for a given device.
    """
    mount_drive(options, args, mount_options, device)
    mount_file(options, args, mount_options, device, device + '1', 'data1')
    mount_file(options, args, mount_options, device, device + '2', 'data2')

def mount_device(options, args):
    """
    Mounts a specific device or all devices based on the provided options.
    """
    mount_options = ''
    if options.utf8:
        mount_options = 'utf8'
    if options.readonly:
        mount_options = ",".join(('ro', mount_options))

    # Mount a specific device if provided
    if args:
        device = args[0]
        if len(args) > 1 and args[1] in ['unmount', 'umount']:
            unmount_device(device)
        else:
            partition(options, args, mount_options, device)
    else:
        partition(options, args, mount_options, 'sdb')
        if options.all:
            mount_all(options, args, mount_options)

def unmount_device(device):
    """
    Unmounts a specified device.
    """
    cmd = 'sudo truecrypt -d ~/mnt/' + device
    os_exec(cmd)

def mount_all(options, args, mount_options):
    """
    Mounts all devices from sdc to sdz.
    """
    for device_suffix in range(ord('c'), ord('z') + 1):
        partition(options, args, mount_options, 'sd' + chr(device_suffix))

def mount_legacy(options, args):
    """
    Mounts legacy devices.
    """
    legacy_devices = ['pc98a', 'pc98b', 'data1', 'data2']
    for device in legacy_devices:
        mount_local(device, options)

def mount_local(device, options):
    """
    Mounts a local TrueCrypt volume.
    """
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
    os_exec(cmd)

def tcmount(options, args):
    """
    Main function to handle the mounting process based on user inputs.
    """
    mount_local('`/bin/hostname`', options)
    if options.legacy or options.all:
        mount_legacy(options, args)
    if not options.local:
        mount_device(options, args)

def main():
    if not is_truecrypt_installed():
        print(
            "Error: TrueCrypt is not installed. This script requires TrueCrypt to mount and unmount encrypted devices. Please install TrueCrypt and try again.")
        sys.exit(5)

    tcmount_version = "3.1"
    truecrypt_version = get_truecrypt_version()

    version_message = "tcmount.py {} - This script operates with {}.".format(
        tcmount_version, truecrypt_version)

    parser = OptionParser(version=version_message)
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

    tcmount(options, args)


if __name__ == "__main__":
    main()

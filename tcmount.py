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
#  v3.2 2023-12-14
#       Removed -l (local), -g (legacy), -f (half), and -p (partition) options.
#       Refactored code to focus on essential mounting functionalities.
#       Added -e (--expansion) option to mount ~/mnt/Expansion/container.tc to a specified device.
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
    cmd = 'test -b /dev/' + device +\
        ' && sudo truecrypt -t -k "" --protect-hidden=no --fs-options=' +\
        mount_options + ' /dev/' + device + ' ~/mnt/' + device
    os_exec(cmd)

def mount_device(options, args, mount_options):
    """
    Mounts a specific device or all devices based on the provided options.
    """

    # Mount a specific device if provided
    if args:
        device = args[0]
        if len(args) > 1 and args[1] in ['unmount', 'umount']:
            unmount_device(device)
        else:
            mount_drive(options, args, mount_options, device)
    else:
        mount_drive(options, args, mount_options, 'sdb')
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
        mount_drive(options, args, mount_options, 'sd' + chr(device_suffix))

def mount_expansion(options, device, mount_options):
    """
    Mounts the ~/mnt/Expansion/container.tc file to the specified device.
    """

    expansion_file = os.path.expanduser('~/mnt/Expansion/container.tc')
    mount_point = os.path.join('~/mnt', device)

    if os.path.exists(expansion_file):
        cmd = 'sudo truecrypt -t -k "" --protect-hidden=no --fs-options={} {} {}'.format(
            mount_options, expansion_file, mount_point)
        os_exec(cmd)
    else:
        print("The expansion file does not exist: {}".format(expansion_file))

def main():
    """
    Main function to handle the mounting process based on user inputs.
    """
    if not is_truecrypt_installed():
        print(
            "Error: TrueCrypt is not installed. This script requires TrueCrypt to mount and unmount encrypted devices. Please install TrueCrypt and try again.")
        sys.exit(5)

    tcmount_version = "3.2"
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
    parser.add_option("-e", "--expansion",
                      dest="expansion",
                      help="Mount the specified device with Expansion",
                      action="store",
                      type="string")

    (options, args) = parser.parse_args()

    mount_options = ''
    if options.utf8:
        mount_options = 'utf8'
    if options.readonly:
        mount_options = ",".join(('ro', mount_options))

    if options.expansion:
        mount_expansion(options, options.expansion, mount_options)
    else:
        mount_device(options, args, mount_options)


if __name__ == "__main__":
    main()

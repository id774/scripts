#!/usr/bin/env python

########################################################################
# tcmount.py: TrueCrypt/VeraCrypt Device Mounter
#
#  Description:
#  This script is designed to automate the mounting and unmounting of
#  TrueCrypt and VeraCrypt encrypted devices. It checks for the presence
#  of the TrueCrypt and VeraCrypt commands and supports a variety of devices,
#  including options for different file systems and encoding types. This version
#  allows for specific device mounting and unmounting by specifying the device
#  name as an argument and choosing between TrueCrypt and VeraCrypt.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v4.0 2023-12-15
#       Added support for VeraCrypt with the -v (--veracrypt) option.
#       Improved error handling for systems where only TrueCrypt or VeraCrypt is installed.
#       Reversed the behavior of the -u (--utf8) option. Now, by default,
#       the filesystem is mounted with UTF-8 encoding, and the -u option
#       is used to disable this setting.
#       Refactored command construction to improve testability.
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
#  To use this script, ensure you have TrueCrypt or VeraCrypt installed and
#  run the script with appropriate privileges. You can specify the device
#  and other mount options as arguments. For example:
#
#      python tcmount.py [device] [options]
#
#  Specific device mounting:
#      python tcmount.py sdb
#      This will mount the device /dev/sdb using TrueCrypt or VeraCrypt based on the options.
#
#  Specific device unmounting:
#      python tcmount.py sdb unmount
#      python tcmount.py sdb umount
#      These commands will unmount the device /dev/sdb using TrueCrypt or VeraCrypt.
#
#  Options:
#  -v, --veracrypt    Use VeraCrypt instead of TrueCrypt for mounting and unmounting.
#  -u, --no-utf8      Do not use UTF-8 encoding for the mounted filesystem.
#  -r, --readonly     Mount the filesystem in read-only mode.
#  -a, --all          Mount all available devices.
#  -e, --expansion    Mount the container file of Expansion to a specified device.
#
#  Refer to the TrueCrypt and VeraCrypt documentation for more detailed information
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

def is_veracrypt_installed():
    """
    Checks if VeraCrypt is installed by searching for its command in the system path.
    """
    return subprocess.call(['which', 'veracrypt'], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL) == 0

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

def get_veracrypt_version():
    """
    Retrieves the version information of VeraCrypt.
    """
    try:
        output = subprocess.check_output(
            ["veracrypt", "--version"], stderr=subprocess.STDOUT)
        return output.decode().strip()
    except subprocess.CalledProcessError:
        return "Unknown"

def build_mount_command(device, mount_options):
    """
    Builds the command to mount a TrueCrypt volume on a specific drive.
    """
    return 'test -b /dev/{0} && sudo truecrypt -t -k "" --protect-hidden=no --fs-options={1} /dev/{0} ~/mnt/{0}'.format(device, mount_options)

def build_unmount_command(device):
    """
    Builds the command to unmount a specified device.
    """
    return 'sudo truecrypt -d ~/mnt/{0}'.format(device)

def build_mount_all_command(mount_options):
    """
    Builds the commands to mount all devices from sdc to sdz.
    """
    commands = []
    for device_suffix in range(ord('c'), ord('z') + 1):
        commands.append(build_mount_command(
            'sd' + chr(device_suffix), mount_options))
    return commands

def build_mount_expansion_command(device, mount_options):
    """
    Builds the command to mount the container.tc file of Expansion to the specified device.
    """
    expansion_file = '~/mnt/Expansion/container.tc'
    mount_point = os.path.join('~/mnt', device)

    return 'test -f {0} && sudo truecrypt -t -k "" --protect-hidden=no --fs-options={1} {0} {2}'.format(expansion_file, mount_options, mount_point)

def process_mounting(options, args):
    """
    Processes the mounting based on the provided options and arguments.
    """
    mount_options = []
    if not options.no_utf8:
        mount_options.append('utf8')
    if options.readonly:
        mount_options.append('ro')

    mount_options_str = ','.join(mount_options)

    commands = []
    if options.expansion:
        cmd = build_mount_expansion_command(
            options.expansion, mount_options_str)
        if cmd:
            commands.append(cmd)
    else:
        if args:
            device = args[0]
            if len(args) > 1 and args[1] in ['unmount', 'umount']:
                commands.append(build_unmount_command(device))
            else:
                commands.append(build_mount_command(device, mount_options_str))
        else:
            commands.append(build_mount_command('sdb', mount_options_str))
            if options.all:
                commands.extend(build_mount_all_command(mount_options_str))

    if options.veracrypt:
        if not is_veracrypt_installed():
            print("Error: VeraCrypt is not installed, but '-v' option was specified. Please use TrueCrypt or install VeraCrypt and try again.")
            sys.exit(6)
        encryption_tool = "veracrypt -tc"
        unmount_cmd = "veracrypt"
    else:
        if not is_truecrypt_installed():
            print(
                "Error: TrueCrypt is not installed. Please use VeraCrypt or install TrueCrypt and try again.")
            sys.exit(6)
        encryption_tool = "truecrypt"
        unmount_cmd = "truecrypt"

    for cmd in commands:
        if 'unmount' in cmd or 'umount' in cmd:
            cmd = cmd.replace('truecrypt', unmount_cmd)
        else:
            cmd = cmd.replace('truecrypt', encryption_tool)
        os_exec(cmd)

def main():
    """
    Main function to handle the mounting process based on user inputs.
    """
    tcmount_version = "4.0"

    versions = []
    if is_truecrypt_installed():
        versions.append(get_truecrypt_version())
    if is_veracrypt_installed():
        versions.append(get_veracrypt_version())

    if not versions:
        print("Error: Neither TrueCrypt nor VeraCrypt is installed. Please install one of them and try again.")
        sys.exit(5)

    version_message = "tcmount.py {} - This script operates with {}.".format(
        tcmount_version, " / ".join(versions))

    parser = OptionParser(version=version_message)
    parser.add_option("-v", "--veracrypt",
                      dest="veracrypt",
                      help="Use VeraCrypt instead of TrueCrypt",
                      action="store_true")
    parser.add_option("-u", "--no-utf8",
                      dest="no_utf8",
                      help="do not use utf8 as the mount filesystem type",
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

    process_mounting(options, args)


if __name__ == "__main__":
    main()

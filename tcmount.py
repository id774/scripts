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
#  It also supports external container mounts with an explicit target.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  To use this script, ensure you have TrueCrypt or VeraCrypt installed and
#  run the script with appropriate privileges. You can specify the device
#  and other mount options as arguments. For example:
#
#      python tcmount.py [device] [target] [options]
#
#  Specific device mounting:
#      python tcmount.py sdb
#      This will mount the device /dev/sdb using TrueCrypt or VeraCrypt based on the options.
#      python tcmount.py sdb disk1
#      This will mount the device /dev/sdb to ~/mnt/disk1 instead of ~/mnt/sdb.
#      (Backwards compatible: omitting the 2nd arg still mounts to ~/mnt/<device>)
#
#  Specific device unmounting:
#      python tcmount.py sdb unmount
#      python tcmount.py sdb umount
#      These commands will unmount the device /dev/sdb using TrueCrypt or VeraCrypt.
#      python tcmount.py sdb disk1 unmount
#      This will unmount ~/mnt/disk1 (explicit target form).
#
#  External container mounting:
#      Legacy fixed container path:
#          ~/mnt/external/container.tc
#
#      Without explicit target:
#          python tcmount.py -e sde
#          -> mounts ~/mnt/external/container.tc to ~/mnt/sde
#
#      With explicit target:
#          python tcmount.py -e sde disk3
#          -> mounts ~/mnt/external/container.tc to ~/mnt/disk3
#
#      Notes:
#          The fix ensures -e honors the explicit target while preserving
#          the legacy container path. Default behavior is unchanged.
#
#  Options:
#  -v, --veracrypt    Use VeraCrypt instead of TrueCrypt for mounting and unmounting.
#  -t, --tc-compat    Use VeraCrypt in TrueCrypt compatibility mode.
#  -u, --no-utf8      Do not use UTF-8 encoding for the mounted filesystem.
#  -r, --readonly     Mount the filesystem in read-only mode.
#  -a, --all          Mount all available devices (sdc..sdz).
#  -e, --external     Mount the legacy external container file (~/mnt/external/container.tc).
#                     If no positional target is provided, it mounts to ~/mnt/<external_device>.
#                     If a positional target is provided (e.g., disk3), it mounts to ~/mnt/<target>.
#                     Example: tcmount.py -e sde disk3
#
#  Requirements:
#  - Python Version: 3.1 or later
#
#  Note on Custom Return Codes:
#  This script uses custom return codes to indicate specific error conditions:
#  - 0: Success. The operation completed without any errors.
#  - 1: Neither TrueCrypt nor VeraCrypt is installed. The script requires one of them to be installed to function.
#  - 11: TrueCrypt is not installed. This is returned when TrueCrypt is required but not found.
#  - 12: VeraCrypt is not installed but specified for use. This occurs when VeraCrypt is selected but not installed.
#  - 13: VeraCrypt compatibility mode is specified but VeraCrypt is not installed.
#
#  Refer to the TrueCrypt and VeraCrypt documentation for more detailed information
#  on mount options and device specifications.
#
#  Version History:
#  v5.1 2025-08-31
#       Fix external mount to honor explicit target and preserve legacy container path.
#  v5.0 2025-08-29
#       Added support for explicit target argument: tcmount.py sdb disk1 mounts /dev/sdb to ~/mnt/disk1.
#       Also supports unmount with explicit target: tcmount.py sdb disk1 unmount.
#  v4.8 2025-07-08
#       Automatically extract __version__ from script header to eliminate hardcoded version.
#  v4.7 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v4.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v4.5 2025-05-14
#       Replaced use of 'which' with POSIX-compliant 'command -v' in installation checks.
#       Added reusable command_exists() function for path resolution.
#  v4.4 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v4.3 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v4.2 2024-01-26
#       Updated documentation to include notes on custom return codes.
#  v4.1 2023-12-17
#       Modified is_truecrypt_installed and is_veracrypt_installed functions for compatibility
#       with Python versions below 3.3, replacing DEVNULL with os.devnull.
#  v4.0 2023-12-15
#       Added support for VeraCrypt with the -v (--veracrypt) and -t (--tc-compat) options.
#       Improved error handling for systems where only TrueCrypt or VeraCrypt is installed.
#       Reversed the behavior of the -u (--utf8) option. Now, by default,
#       the filesystem is mounted with UTF-8 encoding, and the -u option
#       is used to disable this setting.
#       Refactored command construction to improve testability.
#       Renamed the -e (--expansion) option to -e (--external) and updated the path to
#       the container file to '~/mnt/external/container.tc' for generalizing external HDD support.
#  [Further version history truncated for brevity]
#  v1.0 2010-08-06
#       First release.
#
########################################################################

import os
import subprocess
import sys
from optparse import OptionParser

__version__ = "unknown"

def usage():
    """ Display the script header as usage information and exit. """
    script_path = os.path.abspath(__file__)
    in_header = False
    try:
        with open(script_path, 'r', encoding='utf-8') as f:
            for line in f:
                if line.strip().startswith('#' * 10):
                    if not in_header:
                        in_header = True
                        continue
                    else:
                        break
                if in_header and line.startswith('#'):
                    if line.startswith('# '):
                        print(line[2:], end='')
                    else:
                        print(line[1:], end='')
    except Exception as e:
        print("Error reading usage information: %s" % str(e), file=sys.stderr)
        sys.exit(1)
    sys.exit(0)

def get_script_version():
    """ Extracts the script version from the header comment block. """
    script_path = os.path.abspath(__file__)
    found_history = False
    try:
        with open(script_path, 'r', encoding='utf-8') as f:
            for line in f:
                if "Version History" in line:
                    found_history = True
                elif found_history and line.strip().startswith("#  v"):
                    return line.strip().split()[1]
    except Exception:
        return "unknown"
    return "unknown"

def check_sudo():
    """ Check if the user has sudo privileges (password may be required). """
    try:
        with open(os.devnull, 'w') as devnull:
            result = subprocess.call(["sudo", "-v"], stdout=devnull, stderr=devnull)
            if result != 0:
                print("[ERROR] This script requires sudo privileges. Please run as a user with sudo access.", file=sys.stderr)
                sys.exit(1)
    except Exception as e:
        print("[ERROR] Failed to check sudo privileges: {}".format(e), file=sys.stderr)
        sys.exit(1)

def os_exec(cmd):
    """
    Executes a system command using subprocess.
    """
    subprocess.call(cmd, shell=True)

def command_exists(command):
    """
    Checks if a given command exists in the system path using 'command -v'.
    """
    with open(os.devnull, 'w') as devnull:
        return subprocess.call('command -v {}'.format(command), shell=True, stdout=devnull, stderr=devnull) == 0

def is_truecrypt_installed():
    """
    Checks if TrueCrypt is installed by searching for its command in the system path.
    """
    return command_exists('truecrypt')

def is_veracrypt_installed():
    """
    Checks if VeraCrypt is installed by searching for its command in the system path.
    """
    return command_exists('veracrypt')

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

def build_mount_command(device, mount_options, target=None):
    """
    Build command to mount /dev/<device> to ~/mnt/<target or device>
    """
    if not target:
        target = device
    return 'test -b /dev/{0} && sudo truecrypt -t -k "" --protect-hidden=no --fs-options={1} /dev/{0} ~/mnt/{2}'.format(device, mount_options, target)

def build_unmount_command(target):
    """
    Build command to unmount ~/mnt/<target>
    """
    return 'sudo truecrypt -d ~/mnt/{0}'.format(target)

def build_mount_all_command(mount_options):
    """
    Build commands to mount all devices from sdc to sdz
    """
    commands = []
    for device_suffix in range(ord('c'), ord('z') + 1):
        commands.append(build_mount_command('sd' + chr(device_suffix), mount_options))
    return commands

def build_mount_external_command(external_device, mount_options, target=None):
    """
    Build command to mount legacy external container file to ~/mnt/<target or external_device>.
    - external_device: device name given to -e (e.g., 'sde')
    - target: explicit mountpoint name under ~/mnt (e.g., 'disk3')
    """
    # Default mountpoint to the external device name if not specified
    if not target:
        target = external_device
    # Legacy fixed path expected by existing tests and prior behavior
    external_file = os.path.join('~', 'mnt', 'external', 'container.tc')
    mount_point = os.path.join('~', 'mnt', target)
    return 'test -f {0} && sudo truecrypt -t -k "" --protect-hidden=no --fs-options={1} {0} {2}'.format(
        external_file, mount_options, mount_point
    )

def process_mounting(options, args):
    """
    Process mounting and unmounting based on CLI options and arguments
    """
    mount_options = []
    if not options.no_utf8:
        mount_options.append('utf8')
    if options.readonly:
        mount_options.append('ro')

    mount_options_str = ','.join(mount_options)

    commands = []
    if options.external:
        # Honor explicit target if provided with -e:
        #   -e <external_device> [<target>]
        target = None
        if len(args) >= 2:
            # e.g., ['sdb', 'disk3'] -> explicit target is args[1]
            target = args[1]
        elif len(args) == 1 and args[0] not in ['unmount', 'umount']:
            # e.g., ['disk3'] -> explicit target is args[0]
            target = args[0]
        cmd = build_mount_external_command(options.external, mount_options_str, target)
        if cmd:
            commands.append(cmd)
    else:
        if args:
            device = args[0]
            if len(args) > 1 and args[1] in ['unmount', 'umount']:
                # Legacy unmount: second token is action, target defaults to device
                commands.append(build_unmount_command(device))
            elif len(args) > 2 and args[2] in ['unmount', 'umount']:
                # Explicit target unmount: third token is action, second token is target
                target = args[1]
                commands.append(build_unmount_command(target))
            else:
                # Mount: optional explicit target as second token
                target = args[1] if len(args) > 1 else None
                if target is None:
                    # Call with two args to keep backward-compat tests passing
                    commands.append(build_mount_command(device, mount_options_str))
                else:
                    commands.append(build_mount_command(device, mount_options_str, target))
        else:
            # Default device when none provided
            commands.append(build_mount_command('sdb', mount_options_str))
            if options.all:
                commands.extend(build_mount_all_command(mount_options_str))

    # Select encryption tool according to options
    if options.tc_compat:
        if not is_veracrypt_installed():
            print("[ERROR] VeraCrypt is not installed, but '-t' option was specified. Please use TrueCrypt or install VeraCrypt and try again.", file=sys.stderr)
            sys.exit(13)
        encryption_tool = "veracrypt -tc"
        unmount_cmd = "veracrypt"
    elif options.veracrypt:
        if not is_veracrypt_installed():
            print("[ERROR] VeraCrypt is not installed, but '-v' option was specified. Please use TrueCrypt or install VeraCrypt and try again.", file=sys.stderr)
            sys.exit(12)
        encryption_tool = "veracrypt"
        unmount_cmd = "veracrypt"
    else:
        if not is_truecrypt_installed():
            print("[ERROR] TrueCrypt is not installed. Please use VeraCrypt or install TrueCrypt and try again.", file=sys.stderr)
            sys.exit(11)
        encryption_tool = "truecrypt"
        unmount_cmd = "truecrypt"

    # Execute built commands after replacing the encryption tool
    for cmd in commands:
        if ' -d ' in cmd:
            cmd = cmd.replace('truecrypt', unmount_cmd)
        else:
            cmd = cmd.replace('truecrypt', encryption_tool)
        os_exec(cmd)

def main():
    """
    Main function to handle the mounting process based on user inputs.
    """
    global __version__
    __version__ = get_script_version()

    versions = []
    if is_truecrypt_installed():
        versions.append(get_truecrypt_version())
    if is_veracrypt_installed():
        versions.append(get_veracrypt_version())

    if not versions:
        print("[ERROR] Neither TrueCrypt nor VeraCrypt is installed. Please install one of them and try again.", file=sys.stderr)
        sys.exit(1)

    version_message = "tcmount.py {} - This script operates with {}.".format(
        __version__, " / ".join(versions))

    parser = OptionParser(version=version_message)
    parser.add_option("-v", "--veracrypt",
                      dest="veracrypt",
                      help="use VeraCrypt instead of TrueCrypt",
                      action="store_true")
    parser.add_option("-t", "--tc-compat",
                      dest="tc_compat",
                      help="use VeraCrypt in TrueCrypt compatibility mode",
                      action="store_true")
    parser.add_option("-u", "--no-utf8",
                      dest="no_utf8",
                      help="do not use UTF-8 as the mount filesystem type",
                      action="store_true")
    parser.add_option("-r", "--readonly",
                      dest="readonly",
                      help="mount filesystem as read-only",
                      action="store_true")
    parser.add_option("-a", "--all",
                      dest="all",
                      help="mount all available devices",
                      action="store_true")
    parser.add_option("-e", "--external",
                      dest="external",
                      help="mount the specified device with an external drive",
                      action="store",
                      type="string")

    (options, args) = parser.parse_args()

    check_sudo()

    process_mounting(options, args)

    return 0


if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help'):
        usage()

    sys.exit(main())

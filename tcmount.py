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
#  Note: -v is reserved for --veracrypt here, and --version reports the installed
#  TrueCrypt or VeraCrypt versions, so it requires either tool to be present.
#
#  Requirements:
#  - Python Version: 3.1 or later
#  - TrueCrypt or VeraCrypt
#  - sudo
#  - get-device
#  - get-mountpoint
#
#  Notes on Unmounting:
#  From v5.1 onward, unmount operations always resolve the *real mountpoint*
#  before calling `truecrypt -d`. This is done by invoking the external helper
#  commands `get-device` and `get-mountpoint` under the hood.
#  As a result:
#    - `tcmount.py sdb unmount` and `tcmount.py sdb disk1 unmount` both work
#      even if the logical ~/mnt/<target> differs from the actual mount path.
#    - These helpers must be present in $PATH for unmount to succeed.
#  This change replaces the legacy behavior (`sudo truecrypt -d ~/mnt/<target>`)
#  which could fail when the logical path did not match the actual mountpoint.
#
#  Exit Status:
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
#  v5.2 2026-09-03
#       Eliminated shell=True and shell-string command construction from mount
#       and unmount execution; commands now run as argument lists. Propagated
#       external command failure status to the script's exit status. Unmount
#       now aborts without running the detach command when get-device or
#       get-mountpoint resolution fails. Replaced 'command -v' based lookup
#       with a manual PATH search in command_exists().
#  v5.1 2025-09-01
#       Fix external mount to honor explicit target and preserve legacy container path.
#       Change unmount logic to always resolve real mountpoint via get-device/get-mountpoint.
#       Requires these helper commands to be available in $PATH.
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
import re
import stat
import subprocess
import sys
from optparse import OptionParser

__version__ = "unknown"   # default; will be overwritten in main()
version_message = None

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

def build_version_message():
    """
    Build a human-readable version message once, using detected tools.
    Safe to call anytime.
    """
    versions = []
    if is_truecrypt_installed():
        versions.append(get_truecrypt_version())
    if is_veracrypt_installed():
        versions.append(get_veracrypt_version())
    # __version__ may not be set yet if called very early; fall back to "unknown".
    try:
        ver = __version__
    except NameError:
        ver = "unknown"
    return "tcmount.py {} - This script operates with {}.".format(
        ver, " / ".join(versions) if versions else "no crypto tools detected")

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

def os_exec(argv):
    """
    Executes an argument list without a shell and returns its exit status.
    Returns 1 when the command cannot be executed.
    """
    try:
        return subprocess.call(argv)
    except OSError as e:
        print("[ERROR] Failed to execute %s: %s" % (argv[0], e), file=sys.stderr)
        return 1

def find_command(command):
    """
    Search PATH for command's executable path, using a manual PATH search.
    Return the full path when found, or None when not found.
    """
    for directory in os.environ.get("PATH", "").split(os.pathsep):
        candidate = os.path.join(directory if directory else ".", command)
        if os.path.isfile(candidate) and os.access(candidate, os.X_OK):
            return candidate
    return None

def command_exists(command):
    """
    Checks if a given command exists in the system path using a manual PATH search.
    """
    return find_command(command) is not None

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

def is_block_device(path):
    """ Return True when the path refers to a block device. """
    try:
        return stat.S_ISBLK(os.stat(path).st_mode)
    except OSError:
        return False

def build_mount_argv(tool_argv, device, mount_options, target=None):
    """
    Build the argument list to mount /dev/<device> to ~/mnt/<target or device>.
    tool_argv is the encryption tool invocation, e.g. ['truecrypt'] or
    ['veracrypt', '-tc'].
    """
    if not target:
        target = device
    source = '/dev/' + device
    mount_point = os.path.expanduser(os.path.join('~', 'mnt', target))
    return (['sudo'] + tool_argv +
            ['-t', '-k', '', '--protect-hidden=no', '--fs-options=%s' % mount_options,
             source, mount_point])

def build_mount_external_argv(tool_argv, mount_options, target):
    """
    Build the argument list to mount the legacy external container file to
    ~/mnt/<target>.
    """
    external_file = os.path.expanduser(os.path.join('~', 'mnt', 'external', 'container.tc'))
    mount_point = os.path.expanduser(os.path.join('~', 'mnt', target))
    return (['sudo'] + tool_argv +
            ['-t', '-k', '', '--protect-hidden=no', '--fs-options=%s' % mount_options,
             external_file, mount_point])

def build_detach_argv(tool_argv, mountpoint):
    """
    Build the argument list to detach the given mountpoint.
    tool_argv is the encryption tool invocation used for unmounting.
    """
    return ['sudo'] + tool_argv + ['-d', mountpoint]

def build_unmount_external_argv(tool_argv):
    """
    Build the argument list to unmount the legacy external container by its
    file path.
    """
    external_file = os.path.expanduser(os.path.join('~', 'mnt', 'external', 'container.tc'))
    return build_detach_argv(tool_argv, external_file)

def list_all_devices():
    """ Return the device names covered by --all (sdc..sdz). """
    return ['sd' + chr(c) for c in range(ord('c'), ord('z') + 1)]

def resolve_real_mountpoint(target):
    """
    Resolve the real mountpoint of ~/mnt/<target> via get-device and
    get-mountpoint. Return the mountpoint, or None when resolution fails.
    Neither helper is invoked past the first failure.
    """
    mount_point = os.path.expanduser(os.path.join('~', 'mnt', target))
    try:
        device = subprocess.check_output(['get-device', mount_point]).decode('utf-8').strip()
    except (subprocess.CalledProcessError, OSError):
        return None
    try:
        real_mountpoint = subprocess.check_output(['get-mountpoint', device]).decode('utf-8').strip()
    except (subprocess.CalledProcessError, OSError):
        return None
    return real_mountpoint

def run_single_mount(device, mount_options, tool_argv, target=None):
    """ Mount /dev/<device> after verifying it is a block device. """
    source = '/dev/' + device
    if not is_block_device(source):
        print("[ERROR] Not a block device: %s" % source, file=sys.stderr)
        return 1
    return os_exec(build_mount_argv(tool_argv, device, mount_options, target))

def run_single_unmount(target, unmount_tool_argv):
    """
    Unmount by resolving the real mountpoint via get-device/get-mountpoint.
    Does not run the detach command when resolution fails.
    """
    mountpoint = resolve_real_mountpoint(target)
    if mountpoint is None:
        print("[ERROR] Failed to resolve the real mountpoint for %s." % target, file=sys.stderr)
        return 1
    return os_exec(build_detach_argv(unmount_tool_argv, mountpoint))

def run_external_mount(external_device, mount_options, tool_argv, target=None):
    """
    Mount the legacy external container file to ~/mnt/<target or external_device>.
    """
    if target is None or str(target).strip() == "":
        target = external_device
    external_file = os.path.expanduser(os.path.join('~', 'mnt', 'external', 'container.tc'))
    if not os.path.isfile(external_file):
        print("[ERROR] External container file not found: %s" % external_file, file=sys.stderr)
        return 1
    return os_exec(build_mount_external_argv(tool_argv, mount_options, target))

def run_external_unmount(unmount_tool_argv):
    """ Unmount the legacy external container by its file path. """
    return os_exec(build_unmount_external_argv(unmount_tool_argv))

def run_mount_all(mount_options, tool_argv):
    """
    Mount every device from sdc to sdz, continuing past individual failures.
    Returns 1 if any device failed, 0 only when all succeeded.
    """
    any_failed = False
    for device in list_all_devices():
        if run_single_mount(device, mount_options, tool_argv) != 0:
            any_failed = True
    return 1 if any_failed else 0

def process_mounting(options, args):
    """
    Process mounting and unmounting based on CLI options and arguments.
    Returns the resulting exit status.
    """
    mount_options = []
    if not options.no_utf8:
        mount_options.append('utf8')
    if options.readonly:
        mount_options.append('ro')

    mount_options_str = ','.join(mount_options)

    # Select encryption tool according to options.
    if options.tc_compat:
        if not is_veracrypt_installed():
            print("[ERROR] VeraCrypt is not installed, but '-t' option was specified. Please use TrueCrypt or install VeraCrypt and try again.", file=sys.stderr)
            sys.exit(13)
        mount_tool_argv = ['veracrypt', '-tc']
        unmount_tool_argv = ['veracrypt']
    elif options.veracrypt:
        if not is_veracrypt_installed():
            print("[ERROR] VeraCrypt is not installed, but '-v' option was specified. Please use TrueCrypt or install VeraCrypt and try again.", file=sys.stderr)
            sys.exit(12)
        mount_tool_argv = ['veracrypt']
        unmount_tool_argv = ['veracrypt']
    else:
        if not is_truecrypt_installed():
            print("[ERROR] TrueCrypt is not installed. Please use VeraCrypt or install TrueCrypt and try again.", file=sys.stderr)
            sys.exit(11)
        mount_tool_argv = ['truecrypt']
        unmount_tool_argv = ['truecrypt']

    if options.external:
        # Syntax:
        #   Mount   : -e <external_device> [<target>]
        #   Unmount : -e <external_device> [<target>] unmount|umount
        #
        # Rules:
        # - For unmount: always detach by the container file path for compatibility.
        # - For mount:
        #     * If there are >=2 non-action tokens, use the last one as explicit target.
        #     * If there is exactly 1 non-action token, use it as explicit target
        #       unless it looks like a device name 'sd[a-z]'; otherwise default to <external_device>.
        is_unmount = any(t in ['unmount', 'umount'] for t in args)
        if is_unmount:
            return run_external_unmount(unmount_tool_argv)
        tokens = [t for t in args if t not in ['unmount', 'umount']]
        if len(tokens) >= 2:
            explicit_target = tokens[-1]
        elif len(tokens) == 1:
            explicit_target = tokens[0] if not re.match(r'^sd[a-z]$', tokens[0]) else None
        else:
            explicit_target = None  # falls back to external_device
        return run_external_mount(options.external, mount_options_str, mount_tool_argv, explicit_target)

    if args:
        device = args[0]
        if len(args) > 1 and args[1] in ['unmount', 'umount']:
            # Unmount: second token is action, target defaults to device
            return run_single_unmount(device, unmount_tool_argv)
        elif len(args) > 2 and args[2] in ['unmount', 'umount']:
            # Explicit target unmount: third token is action, second token is target
            target = args[1]
            return run_single_unmount(target, unmount_tool_argv)
        else:
            # Mount: optional explicit target as second token
            target = args[1] if len(args) > 1 else None
            return run_single_mount(device, mount_options_str, mount_tool_argv, target)

    # No positional args: keep --all behavior; otherwise show version and return.
    if options.all:
        return run_mount_all(mount_options_str, mount_tool_argv)

    # Ensure version_message is available (e.g., when called with only flags like -v)
    global version_message
    if not version_message:
        version_message = build_version_message()
    print(version_message)
    return 0

def main():
    """
    Main function to handle the mounting process based on user inputs.
    """
    global __version__, version_message
    __version__ = get_script_version()

    versions = []
    if is_truecrypt_installed():
        versions.append(get_truecrypt_version())
    if is_veracrypt_installed():
        versions.append(get_veracrypt_version())

    if not versions:
        print("[ERROR] Neither TrueCrypt nor VeraCrypt is installed. Please install one of them and try again.", file=sys.stderr)
        sys.exit(1)

    # Build and cache the version message so process_mounting() can print it safely.
    version_message = build_version_message()

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

    return process_mounting(options, args)


if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help'):
        usage()

    sys.exit(main())

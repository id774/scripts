#!/usr/bin/env python

########################################################################
# luksmount.py: LUKS Device Mounter
#
#  Description:
#  This script assists with opening and mounting a LUKS encrypted block
#  device that the user has already selected. Given a device name and a
#  mapper name, it retrieves and displays the serial number of the physical
#  device through `get-serial`, asks the user to confirm the selected
#  device, opens it with `cryptsetup open`, and mounts the resulting
#  mapper on /mnt/user/<name>.
#
#  Device selection (for example with `lsblk -f`) is outside the scope of
#  this script and remains a manual step. The mount target is fixed to
#  /mnt/user/<name>. This script does not unmount volumes and does not
#  close mappings.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      luksmount.py <device> <name>
#
#  Example:
#      luksmount.py sdb disk3
#
#  This opens /dev/sdb as /dev/mapper/disk3 and mounts it on /mnt/user/disk3.
#
#  Options:
#  -h, --help       Display this help message and exit.
#  -v, --version    Display the script version and exit.
#
#  Requirements:
#  - Linux
#  - Python Version: 3.1 or later
#  - get-serial
#  - cryptsetup
#  - mount
#  - sudo
#
#  Notes:
#  - Specify the device without the /dev/ prefix (e.g. sdb, not /dev/sdb).
#  - The name is used both as the mapper name and as the mount directory
#    name under /mnt/user.
#  - The mount point directory /mnt/user/<name> must already exist.
#    This script does not create mount points.
#  - Nothing is changed on the system until the user answers 'y' to the
#    confirmation prompt after the serial number is displayed.
#  - If mount fails after the device has been opened, the mapper remains
#    open. This script does not close mappers.
#
#  Exit Status:
#  0: Success, help/version display, or user cancellation.
#  1: General failure.
#  126: Required command exists but is not executable.
#  127: Required command is not installed.
#
#  Version History:
#  v1.0 2026-09-03
#       Initial release. Suppress sudo -v output while keeping the interactive
#       password prompt, and fix find_command() to search the current directory
#       for an empty PATH component instead of skipping it.
#
########################################################################

import os
import re
import stat
import subprocess
import sys

REQUIRED_COMMANDS = ["get-serial", "cryptsetup", "mount", "sudo"]
IDENTIFIER_PATTERN = re.compile(r'^[A-Za-z0-9][A-Za-z0-9._-]*$')
MOUNT_ROOT = "/mnt/user"


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
    """ Extract the script version from the header comment block. """
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


def find_command(command):
    """
    Search PATH for the given command.

    Return a tuple (path, status). The status is 0 when an executable
    candidate is found, 126 when a same-named file exists on PATH but no
    candidate is executable, and 127 when no candidate exists at all.
    """
    found_non_executable = False
    for directory in os.environ.get("PATH", "").split(os.pathsep):
        candidate = os.path.join(directory if directory else ".", command)
        if not os.path.isfile(candidate):
            continue
        if os.access(candidate, os.X_OK):
            return candidate, 0
        found_non_executable = True
    if found_non_executable:
        return None, 126
    return None, 127


def check_required_commands(commands):
    """ Verify that every required command is available and return the process status. """
    for command in commands:
        path, status = find_command(command)
        if status == 126:
            print("[ERROR] Command '%s' is not executable. Please check the permissions." % command, file=sys.stderr)
            return 126
        if status == 127:
            print("[ERROR] Command '%s' is not installed. Please install %s and try again." % (command, command), file=sys.stderr)
            return 127
    return 0


def is_valid_identifier(value):
    """ Return True when the value matches the allowed device / name pattern. """
    return IDENTIFIER_PATTERN.match(value) is not None


def build_paths(device, name):
    """ Return the (source, mapper, target) paths derived from device and name. """
    source = "/dev/" + device
    mapper = "/dev/mapper/" + name
    target = MOUNT_ROOT + "/" + name
    return source, mapper, target


def is_block_device(path):
    """ Return True when the path refers to a block device. """
    try:
        return stat.S_ISBLK(os.stat(path).st_mode)
    except OSError:
        return False


def validate_paths(source, mapper, target):
    """ Verify the source, mapper, and target paths before any state change; return 0 or 1. """
    if not os.path.exists(source):
        print("[ERROR] Device not found: %s" % source, file=sys.stderr)
        return 1
    if not is_block_device(source):
        print("[ERROR] Path is not a block device: %s" % source, file=sys.stderr)
        return 1
    if os.path.exists(mapper):
        print("[ERROR] Mapper already exists: %s" % mapper, file=sys.stderr)
        return 1
    if not os.path.isdir(target):
        print("[ERROR] Mount point not found: %s" % target, file=sys.stderr)
        return 1
    return 0


def get_serial(source):
    """ Run get-serial on the source device and return the serial, or None on failure. """
    try:
        proc = subprocess.Popen(["get-serial", source], stdout=subprocess.PIPE)
        out, _ = proc.communicate()
    except OSError:
        return None
    if proc.returncode != 0:
        return None
    return out.decode('utf-8').rstrip('\n')


def confirm(source, serial, mapper, target):
    """ Show the operation targets and return True only when the user answers 'y'. """
    print("[INFO] Device: %s" % source)
    print("[INFO] Serial: %s" % serial)
    print("[INFO] Mapper: %s" % mapper)
    print("[INFO] Mount point: %s" % target)
    try:
        answer = input("Proceed? [y/N]: ")
    except EOFError:
        return False
    return answer.strip().lower() == 'y'


def check_sudo():
    """ Return True when the user has sudo privileges (password may be required). """
    try:
        with open(os.devnull, 'w') as devnull:
            status = subprocess.call(["sudo", "-v"], stdout=devnull, stderr=devnull)
    except OSError as e:
        print("[ERROR] Failed to execute sudo: %s" % e, file=sys.stderr)
        return False
    if status != 0:
        print("[ERROR] This script requires sudo privileges. Please run as a user with sudo access.", file=sys.stderr)
        return False
    return True


def build_open_command(source, name):
    """ Return the argument list used to open the LUKS device. """
    return ["sudo", "cryptsetup", "open", source, name]


def build_mount_command(mapper, target):
    """ Return the argument list used to mount the mapper on the target. """
    return ["sudo", "mount", mapper, target]


def run_command(command):
    """ Execute an argument list without a shell and return its exit status. """
    try:
        return subprocess.call(command)
    except OSError as e:
        print("[ERROR] Failed to execute %s: %s" % (command[0], e), file=sys.stderr)
        return 1


def process_mount(device, name):
    """ Run validation, serial display, confirmation, sudo check, open, and mount in order. """
    source, mapper, target = build_paths(device, name)

    status = validate_paths(source, mapper, target)
    if status != 0:
        return status

    serial = get_serial(source)
    if serial is None:
        print("[ERROR] Failed to determine serial number for %s." % source, file=sys.stderr)
        return 1

    if not confirm(source, serial, mapper, target):
        print("[INFO] Operation cancelled.")
        return 0

    if not check_sudo():
        return 1

    print("[INFO] Opening %s as %s." % (source, name))
    if run_command(build_open_command(source, name)) != 0:
        print("[ERROR] Failed to open %s as %s." % (source, name), file=sys.stderr)
        return 1

    print("[INFO] Mounting %s on %s." % (mapper, target))
    if run_command(build_mount_command(mapper, target)) != 0:
        print("[ERROR] Failed to mount %s on %s." % (mapper, target), file=sys.stderr)
        print("[WARN] %s remains open." % mapper, file=sys.stderr)
        return 1

    print("[INFO] Mounted %s on %s." % (mapper, target))
    return 0


def main():
    """ Handle help, version, and argument validation, then run the mount workflow. """
    args = sys.argv[1:]

    if len(args) == 1 and args[0] in ('-h', '--help'):
        usage()
    if len(args) == 1 and args[0] in ('-v', '--version'):
        print("luksmount.py %s" % get_script_version())
        return 0

    if len(args) != 2 or args[0].startswith('-') or args[1].startswith('-'):
        print("Usage: luksmount.py <device> <name>", file=sys.stderr)
        return 1

    device, name = args
    if not is_valid_identifier(device):
        print("[ERROR] Invalid device name: %s" % device, file=sys.stderr)
        return 1
    if not is_valid_identifier(name):
        print("[ERROR] Invalid mapper name: %s" % name, file=sys.stderr)
        return 1

    status = check_required_commands(REQUIRED_COMMANDS)
    if status != 0:
        return status

    return process_mount(device, name)


if __name__ == '__main__':
    sys.exit(main())

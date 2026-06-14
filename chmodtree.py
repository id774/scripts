#!/usr/bin/env python

########################################################################
# chmodtree.py: Directory Tree Permission and Ownership Normalization Script
#
#  Description:
#  This script normalizes file and directory permissions, owner, and group
#  recursively within a specified directory tree.
#
#  It uses the 'find' command to select matching entries and applies 'chmod'
#  and/or 'chown' in batches with 'find -exec ... {} +'. This avoids invoking
#  chmod or chown once per path and keeps large directory trees practical.
#
#  By default, this script updates only entries whose current attributes differ
#  from the requested values:
#      - Numeric chmod modes use find '! -perm MODE' before invoking chmod.
#      - Owner and group normalization use find '! -user' and '! -group' before
#        invoking chown.
#
#  This default behavior reduces unnecessary chmod/chown calls, ctime updates,
#  inode metadata writes, and filesystem journal updates during repeated daily
#  maintenance runs. Use --force when all matched entries must be processed
#  regardless of their current attributes.
#
#  Symbolic chmod modes such as 'u+rw', 'g-w', or 'a+rX' are passed directly to
#  chmod, but they are not pre-filtered with '! -perm' because their "already
#  matched" state cannot be safely represented as a simple exact permission
#  comparison. Numeric modes such as 0644, 0755, 1777, and 2755 are pre-filtered
#  unless --force is specified.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script with a directory and options:
#      chmodtree.py [options] dir
#
#  Options:
#    -h, --help            show this help message and exit
#    -s, --sudo            exec with sudo
#    -q, --quiet           shut off non-error messages
#    -f FILES, --files=FILES
#                          chmod files
#    -d DIRS, --dirs=DIRS  chmod directories
#    -n NAME, --name=NAME  name pattern of find (ex. -n '*.sh')
#    --user=USER           normalize owner with chown
#    --group=GROUP         normalize group with chown
#    --force               apply chmod/chown to all matched entries without
#                          skipping entries that already match
#
#  Options include --sudo to execute with superuser privileges, --quiet to
#  reduce output verbosity, --files and --dirs to specify chmod permissions for
#  files and directories, --user and --group to normalize ownership, --name to
#  filter by filename pattern, and --force to restore the previous all-matched
#  reapplication behavior.
#
#  Permission modes are passed directly to chmod, so both zero-prefixed forms
#  such as 0644/0755 and non-prefixed forms such as 644/755 can be used.
#  This script does not validate permission mode strings before passing them to
#  chmod. However, only simple octal modes are used for the default '! -perm'
#  pre-filtering. Invalid values are passed to chmod as-is and chmod errors are
#  propagated to the script exit status.
#
#  The --user and --group values are passed to chown and find as provided.
#  Invalid user or group names are reported by find/chown and their failures are
#  propagated to the script exit status.
#
#  Command construction is performed with argument lists rather than shell
#  strings. This avoids shell interpretation of directory names, name patterns,
#  permission modes, users, and groups.
#
#  Example:
#      Normalize file permissions only, matching a specific pattern:
#          chmodtree.py -f 0644 -n '*.py' your_dir1
#      Normalize directory permissions only, using sudo:
#          chmodtree.py -s -d 0755 your_dir2
#      Normalize files and directories with different permissions:
#          chmodtree.py -f 0644 -d 0755 your_dir3
#      Normalize permissions and ownership for a data tree:
#          chmodtree.py -s -q -f 0644 -d 0755 --user root --group root your_dir4
#      Normalize executable shell scripts only:
#          chmodtree.py -f 0755 -n '*.sh' your_dir5
#      Reapply chmod/chown to all matched entries even if already correct:
#          chmodtree.py --force -f 0644 -d 0755 --user root --group root your_dir6
#
#  Requirements:
#  - Python Version: 3.1 or later
#  - External commands: find, chmod, chown when --user or --group is used
#
#  Version History:
#  v3.1 2026-06-14
#       Added owner and group normalization support with --user and --group.
#       Changed the default behavior to update only entries whose current
#       permissions, owner, or group differ from the requested values.
#       Added --force to reapply chmod/chown to all matched entries.
#       Expanded documentation for batching, default skip behavior, symbolic
#       mode limitations, and command safety.
#  v3.0 2026-06-13
#       Improved performance for large directory trees by batching chmod execution
#       with find -exec ... {} + instead of invoking chmod once per path.
#       Replaced shell command execution with argument-list execution to avoid shell
#       interpretation and propagate command failures to the script exit status.
#       Fixed Linux platform detection for chmod -c on Python 3.
#  v2.8 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v2.7 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v2.6 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v2.5 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v2.4 2024-01-28
#       Replaced shutil.which with a custom which function to ensure compatibility
#       with Python versions prior to 3.3.
#  v2.3 2024-01-20
#       Refactored to include a main function and separate option parser setup function.
#  v2.2 2024-01-18
#       Standardized command existence checks using a common function.
#  v2.1 2023-12-17
#       Updated for compatibility with Python versions below 3.3 by replacing
#       DEVNULL with os.devnull in is_command_installed function.
#  v2.0 2023-12-16
#       Refactored to use command construction method. Added external command checks.
#       Updated documentation and added comments to functions.
#  v1.2 2014-08-14
#       Minor formatting revisions for readability and consistency.
#  v1.1 2009-01-27
#       Enhanced compatibility to consider platforms of Python other than GNU/Linux.
#  v1.0 2009-01-26
#       Initial release.
#
########################################################################

import os
import subprocess
import sys
from optparse import OptionParser


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


def setup_option_parser():
    """ Initialize and return an argument parser for command-line options. """
    parser = OptionParser("usage: %prog [options] dir")
    parser.add_option("-s", "--sudo", help="exec with sudo",
                      action="store_true", dest="sudo")
    parser.add_option("-q", "--quiet", help="shut off non-error messages",
                      action="store_true", dest="quiet")
    parser.add_option("-f", "--files", dest="files", help="chmod files")
    parser.add_option("-d", "--dirs", dest="dirs", help="chmod directories")
    parser.add_option("-n", "--name", dest="name",
                      help="name pattern of find (ex. -n '*.sh')")
    parser.add_option("--user", dest="user", help="normalize owner with chown")
    parser.add_option("--group", dest="group", help="normalize group with chown")
    parser.add_option("--force", help="apply chmod/chown to all matched entries",
                      action="store_true", dest="force")
    return parser


def find_command(cmd):
    """ Check if a given command exists in the system's PATH. """
    for path in os.environ.get("PATH", "").split(os.pathsep):
        full_path = os.path.join(path, cmd)
        if os.path.isfile(full_path):
            return full_path
    return None


def check_command(cmd):
    """ Verify if a command is available and executable in the system's PATH. """
    cmd_path = find_command(cmd)
    if not cmd_path:
        # If the command is not found
        print("[ERROR] Command '{}' is not installed. Please install {} and try again.".format(cmd, cmd), file=sys.stderr)
        sys.exit(127)
    elif not os.access(cmd_path, os.X_OK):
        # If the command is found but not executable
        print("[ERROR] Command '{}' is not executable. Please check the permissions.".format(cmd), file=sys.stderr)
        sys.exit(126)


def os_exec(cmd):
    """ Execute a system command using subprocess. """
    return subprocess.call(cmd)


def is_octal_mode(mode):
    """ Return True if mode is a simple octal permission string. """
    if not mode:
        return False

    if mode.startswith('0'):
        mode = mode[1:]

    return len(mode) in (3, 4) and all(c in '01234567' for c in mode)


def build_find_command(options, directory):
    """ Build the base find command based on provided options and directory. """
    base_cmd = ['find', directory]
    if options.name:
        base_cmd.extend(['-name', options.name])
    return base_cmd


def build_chmod_options(options):
    """ Build chmod verbosity options. """
    chmod_opts = []

    if not options.quiet:
        if sys.platform.startswith('linux'):
            chmod_opts.append('-c')
        else:
            chmod_opts.append('-v')

    return chmod_opts


def build_chown_options(options):
    """ Build chown verbosity options. """
    chown_opts = []

    if not options.quiet:
        if sys.platform.startswith('linux'):
            chown_opts.append('-c')
        else:
            chown_opts.append('-v')

    return chown_opts


def build_chmod_command(base_cmd, options, file_type):
    """ Build the chmod command for either files or directories. """
    mode = options.files if file_type == 'f' else options.dirs
    cmd = list(base_cmd)
    cmd.extend(['-type', file_type])

    if not options.force and is_octal_mode(mode):
        cmd.extend(['!', '-perm', mode])

    cmd.extend(['-exec', 'chmod'])
    cmd.extend(build_chmod_options(options))
    cmd.extend(['--', mode, '{}', '+'])

    return cmd


def build_chown_spec(options):
    """ Build the owner[:group] argument passed to chown. """
    if options.user and options.group:
        return options.user + ':' + options.group
    if options.user:
        return options.user
    if options.group:
        return ':' + options.group
    return None


def build_chown_filter(options):
    """ Build the find predicates used to select entries with mismatched owner or group. """
    if options.force:
        return []

    if options.user and options.group:
        return ['(', '!', '-user', options.user, '-o', '!', '-group', options.group, ')']
    if options.user:
        return ['!', '-user', options.user]
    if options.group:
        return ['!', '-group', options.group]

    return []


def build_chown_command(base_cmd, options):
    """ Build the chown command for owner and/or group normalization. """
    chown_spec = build_chown_spec(options)
    cmd = list(base_cmd)

    cmd.extend(build_chown_filter(options))
    cmd.extend(['-exec', 'chown'])
    cmd.extend(build_chown_options(options))
    cmd.extend(['--', chown_spec, '{}', '+'])

    return cmd


def keep_first_error(status, rc):
    """ Keep the first non-zero command return code. """
    if rc != 0 and status == 0:
        return rc
    return status


def chmodtree(options, directory):
    """ Apply chmod/chown normalization in the given directory based on options. """
    base_cmd = build_find_command(options, directory)
    sudo_prefix = ['sudo'] if options.sudo else []
    status = 0

    if options.files:
        file_cmd = build_chmod_command(base_cmd, options, 'f')
        status = keep_first_error(status, os_exec(sudo_prefix + file_cmd))

    if options.dirs:
        dir_cmd = build_chmod_command(base_cmd, options, 'd')
        status = keep_first_error(status, os_exec(sudo_prefix + dir_cmd))

    if options.user or options.group:
        chown_cmd = build_chown_command(base_cmd, options)
        status = keep_first_error(status, os_exec(sudo_prefix + chown_cmd))

    return status


def main():
    """ Main function to parse options and execute chmodtree. """
    parser = setup_option_parser()
    (options, args) = parser.parse_args()

    if len(args) != 1:
        parser.print_help()
        return 1

    check_command('find')

    if options.files or options.dirs:
        check_command('chmod')

    if options.user or options.group:
        check_command('chown')

    if options.sudo:
        check_sudo()

    return chmodtree(options, args[0])


if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    sys.exit(main())

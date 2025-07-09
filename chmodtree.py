#!/usr/bin/env python

########################################################################
# chmodtree.py: Directory Tree chmod Script
#
#  Description:
#  This script changes file and directory permissions recursively within a specified directory.
#  It uses the 'find' command to apply 'chmod' to files and directories matching certain criteria.
#  The script supports options for using 'sudo', controlling verbosity, and filtering by name patterns.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
#    -d DIRS, --dirs=DIRS  chmod directory
#    -n NAME, --name=NAME  name pattern of find (ex. -n '*.sh')
#
#  Options include --sudo to execute with superuser privileges, --quiet to reduce output verbosity,
#  --files and --dirs to specify chmod permissions for files and directories, and --name to filter
#  by filename pattern.
#
#  Example:
#    Changing permissions of files only, matching a specific pattern:
#      python chmodtree.py -f 644 -n '*.py' your_dir1
#    Changing permissions of directories only, using sudo:
#      python chmodtree.py -s -d 755 your_dir2
#    Changing permissions of both files and directories, matching a specific pattern:
#      python chmodtree.py -s -f 600 -d 700 -n '*.rb' your_dir3
#    Changing permissions with verbose output:
#      python chmodtree.py -s -q -f 640 -d 750 -n '*.txt' your_dir4
#    Changing permissions in quiet mode, matching a specific pattern:
#      python chmodtree.py -f 775 -d 750 -n '*.sh' your_dir5
#
#  Requirements:
#  - Python Version: 3.1 or later
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
    parser.add_option("-d", "--dirs", dest="dirs", help="chmod directory")
    parser.add_option("-n", "--name", dest="name",
                      help="name pattern of find (ex. -n '*.sh')")
    return parser

def find_command(cmd):
    """ Check if a given command exists in the system's PATH. """
    for path in os.environ["PATH"].split(os.pathsep):
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
    subprocess.call(cmd, shell=True)

def build_find_command(options, directory):
    """ Build the find command based on provided options and directory. """
    base_cmd = 'find ' + directory
    if options.name:
        base_cmd += ' -name "{}"'.format(options.name)
    return base_cmd

def build_chmod_command(base_cmd, options, file_type):
    """ Build the chmod command for either files or directories. """
    chmod_cmd = '{} -type {} -exec chmod {}{} {{}} \\;'.format(
        base_cmd, file_type, '-c ' if not options.quiet and sys.platform == 'linux2' else '-v ' if not options.quiet else '',
        options.files if file_type == 'f' else options.dirs)
    return chmod_cmd

def chmodtree(options, directory):
    """ Apply chmod to files and directories in the given directory based on options. """
    base_cmd = build_find_command(options, directory)
    sudo_prefix = 'sudo ' if options.sudo else ''

    if options.files:
        file_cmd = build_chmod_command(base_cmd, options, 'f')
        os_exec(sudo_prefix + file_cmd)

    if options.dirs:
        dir_cmd = build_chmod_command(base_cmd, options, 'd')
        os_exec(sudo_prefix + dir_cmd)

def main():
    """ Main function to parse options and execute chmodtree. """
    check_command('find')
    check_command('chmod')

    parser = setup_option_parser()
    (options, args) = parser.parse_args()

    if len(args) != 1:
        parser.print_help()
    else:
        if options.sudo:
            check_sudo()
        chmodtree(options, args[0])
        return 0


if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()
    sys.exit(main())

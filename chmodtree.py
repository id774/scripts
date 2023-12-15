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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
#      python chmodtree.py [options] dir
#
#  Options include --sudo to execute with superuser privileges, --quiet to reduce output verbosity,
#  --files and --dirs to specify chmod permissions for files and directories, and --name to filter
#  by filename pattern.
#
########################################################################

import sys
import os
from optparse import OptionParser
import subprocess

def is_command_installed(command):
    """ Check if a command is installed on the system. """
    return subprocess.call(['which', command], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL) == 0

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
    if not is_command_installed('find') or not is_command_installed('chmod'):
        sys.exit("Error: Required commands 'find' and/or 'chmod' are not installed.")

    parser = OptionParser("usage: %prog [options] dir")
    parser.add_option("-s", "--sudo", help="exec with sudo",
                      action="store_true", dest="sudo")
    parser.add_option("-q", "--quiet", help="shut off non-error messages",
                      action="store_true", dest="quiet")
    parser.add_option("-f", "--files", dest="files", help="chmod files")
    parser.add_option("-d", "--dirs", dest="dirs", help="chmod directory")
    parser.add_option("-n", "--name", dest="name",
                      help="name pattern of find (ex. -n '*.sh')")
    (options, args) = parser.parse_args()

    if len(args) != 1:
        parser.print_help()
    else:
        chmodtree(options, args[0])


if __name__ == "__main__":
    main()

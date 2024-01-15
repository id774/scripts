#!/usr/bin/env python

########################################################################
# du.py: Simplified Disk Usage Reporting for macOS
#
#  Description:
#  This script provides a simplified disk usage reporting on macOS, similar to
#  the 'du' command with a '--max-depth' option. It reports disk usage at a
#  specified depth and displays the total usage of the top-level directory.
#  The script checks if the specified directory exists and is a directory
#  before performing operations, enhancing error handling.
#  This script is designed to work exclusively on macOS.
#
#  Note:
#  The script focuses on reporting the total disk usage of the specified
#  top-level directory, avoiding the complexities and limitations of the
#  traditional 'du' command on macOS. It does not sum up the sizes of
#  subdirectories; instead, it directly reports the size of the top-level
#  directory as the total.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2024-01-16
#      Added error handling for non-existent or non-directory paths.
#  v1.2 2023-12-30
#      Fixed the issue with incorrect total disk usage calculation.
#      The script now correctly identifies and reports the size of the top directory.
#      Simplified total size calculation by using the size string directly.
#  v1.1 2023-12-25
#      Added total disk usage calculation.
#      Added note regarding total usage variation based on depth.
#  v1.0 2023-12-18
#      Initial release. Simplifies 'du' command usage.
#
#  Usage:
#  Run the script with a depth number and a directory:
#      python du.py [maxdepth] [directory]
#
########################################################################

import sys
import os
import platform
import subprocess

def is_command_exist(command):
    """
    Check if a given command exists in the system.
    """
    return subprocess.call(['which', command], stdout=subprocess.PIPE, stderr=subprocess.PIPE) == 0

def parse_du_output(du_output, directory):
    """
    Parse the output of the 'du' command to find the size of the specified top directory.
    """
    for line in du_output.split('\n'):
        if line:
            size, path = line.split('\t')
            if path.rstrip('/') == directory.rstrip('/'):
                return size.strip()  # Return the size as a string
    return "0B"  # Default return value if not found

def run_custom_du(maxdepth, directory):
    """
    Run a custom 'du' command using 'find' and 'du' with the specified maxdepth.
    Check if the specified directory exists and is a directory.
    """
    # Check if the directory exists
    if not os.path.exists(directory):
        print("Error: Directory '{}' does not exist.".format(directory))
        return
    # Check if the path is a directory
    if not os.path.isdir(directory):
        print("Error: '{}' is not a directory.".format(directory))
        return

    find_command = ['find', directory, '-type', 'd', '-maxdepth',
                    maxdepth, '-exec', 'du', '-h', '-d', '0', '{}', ';']
    result = subprocess.check_output(find_command).decode('utf-8')
    print(result)
    total_size = parse_du_output(result, directory)
    print("Total: " + total_size)

def main(args):
    if platform.system() != 'Darwin':
        print("Error: This script is intended for use on macOS only.")
        sys.exit(1)

    if not is_command_exist('find') or not is_command_exist('du'):
        print("Error: Required commands 'find' or 'du' are not available.")
        sys.exit(2)

    if len(args) >= 2 and args[0].isdigit():
        run_custom_du(args[0], args[1])
    else:
        print("Usage: python du.py [maxdepth] [directory]")


# Main execution
if __name__ == '__main__':
    main(sys.argv[1:])

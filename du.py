#!/usr/bin/env python

########################################################################
# du.py: Simplified Disk Usage Reporting for macOS
#
# Description:
# This script is a workaround for the lack of '--max-depth' option in the
# 'du' command on macOS. It uses a combination of 'find' and 'du' commands
# to simulate the '--max-depth' functionality. The script takes a depth
# number and a directory as arguments to perform disk usage analysis
# with specified depth.
#
# Author: id774 (More info: http://id774.net)
# Source Code: https://github.com/id774/scripts
# License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
# Contact: idnanashi@gmail.com
#
# Version History:
# v1.0 2023-12-18
#     Initial release. Simplifies 'du' command usage.
#
# Usage:
# Run the script with a depth number and a directory:
#     python du.py [maxdepth] [directory]
#
########################################################################

import subprocess
import sys

def is_command_exist(command):
    """
    Check if a given command exists in the system.
    """
    return subprocess.call(['which', command], stdout=subprocess.PIPE, stderr=subprocess.PIPE) == 0

def run_custom_du(maxdepth, directory):
    """
    Run a custom 'du' command using 'find' and 'du' with the specified maxdepth.
    """
    find_command = ['find', directory, '-type', 'd', '-maxdepth',
                    maxdepth, '-exec', 'du', '-h', '-sh', '{}', ';']
    subprocess.call(find_command)

def main(args):
    if not is_command_exist('find') or not is_command_exist('du'):
        print("Error: Required commands 'find' or 'du' are not available.")
        sys.exit(1)

    if len(args) >= 2 and args[0].isdigit():
        run_custom_du(args[0], args[1])
    else:
        print("Usage: python du.py [maxdepth] [directory]")


# Main execution
if __name__ == '__main__':
    main(sys.argv[1:])

#!/usr/bin/env python

########################################################################
# du.py: Simplified Disk Usage Reporting for macOS
#
#  Description:
#  This script is a workaround for the lack of '--max-depth' option in the
#  'du' command on macOS. It reports disk usage at specified depth and
#  also shows the total usage. It works only on macOS.
#
#  Note:
#  The total reported disk usage can vary depending on the specified depth.
#  A greater depth value will include more subdirectories in the calculation,
#  potentially increasing the reported total disk usage.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2023-12-30
#      Fixed the issue with incorrect total disk usage calculation.
#      The script now correctly identifies and reports the size of the top directory.
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

import subprocess
import sys
import platform

def is_command_exist(command):
    """
    Check if a given command exists in the system.
    """
    return subprocess.call(['which', command], stdout=subprocess.PIPE, stderr=subprocess.PIPE) == 0

def parse_du_output(du_output, maxdepth, directory):
    """
    Parse the output of the 'du' command to find the size of the top directory.
    """
    for line in du_output.split('\n'):
        if line:
            size, path = line.split('\t')
            size = size.strip()
            path = path.rstrip('/')  # Removes trailing slash if present

            # Check if the path matches the depth of the specified directory
            if path == directory.rstrip('/') or path.count('/') == directory.rstrip('/').count('/') + int(maxdepth):
                # Convert size to bytes
                if size.endswith('K'):
                    return int(float(size[:-1]) * 1024)
                elif size.endswith('M'):
                    return int(float(size[:-1]) * 1024 * 1024)
                elif size.endswith('G'):
                    return int(float(size[:-1]) * 1024 * 1024 * 1024)
                elif size.endswith('T'):
                    return int(float(size[:-1]) * 1024 * 1024 * 1024 * 1024)
                elif size.endswith('P'):
                    return int(float(size[:-1]) * 1024 * 1024 * 1024 * 1024 * 1024)
                elif size.endswith('B') and size[:-1].isdigit():
                    return int(size[:-1])
    return 0  # Return 0 if no matching top directory size found

def run_custom_du(maxdepth, directory):
    """
    Run a custom 'du' command using 'find' and 'du' with the specified maxdepth.
    """
    find_command = ['find', directory, '-type', 'd', '-maxdepth',
                    maxdepth, '-exec', 'du', '-h', '-d', '0', '{}', ';']
    result = subprocess.check_output(find_command).decode('utf-8')
    print(result)
    total = parse_du_output(result, maxdepth, directory)
    print("Total: {:.2f}G".format(total / (1024 * 1024 * 1024)))

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

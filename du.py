#!/usr/bin/env python

########################################################################
# du.py: Simplified Disk Usage Reporting for macOS
#
#  Description:
#  This script provides a simplified disk usage reporting on macOS,
#  similar to the 'du' command with a '--max-depth' option. It reports
#  disk usage at a specified depth and displays the total usage of the
#  top-level directory. The script now includes an option to include or
#  exclude hidden directories (those starting with '.') in the report.
#  This script is designed to work exclusively on macOS.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script with a depth number and a directory:
#      python du.py [maxdepth] [directory]
#
#  Requirements:
#  - Python Version: 3.2 or later
#
#  Notes:
#  The script focuses on reporting the total disk usage of the specified
#  top-level directory, avoiding the complexities and limitations of the
#  traditional 'du' command on macOS. It does not sum up the sizes of
#  subdirectories; instead, it directly reports the size of the top-level
#  directory as the total.
#
#  Version History:
#  v1.7 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.5 2025-05-15
#       Replaced use of 'which' with POSIX-compliant 'command -v' in installation checks.
#  v1.4 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.3 2024-01-16
#      Added error handling for non-existent or non-directory paths.
#      Added option to include or exclude hidden directories in the report.
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
########################################################################

import argparse
import os
import platform
import subprocess
import sys


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

def command_exists(command):
    """
    Checks if a given command exists in the system path using 'command -v'.
    """
    with open(os.devnull, 'w') as devnull:
        return subprocess.call('command -v {}'.format(command), shell=True, stdout=devnull, stderr=devnull) == 0

def error_message(message, exit_code=1):
    """
    Print an error message and exit the program with the specified exit code.
    This function is used for error handling across the script.
    """
    print("[ERROR] " + message, file=sys.stderr)
    sys.exit(exit_code)

def check_directory(directory):
    """
    Check if the specified directory exists and is actually a directory.
    If the directory does not exist or is not a directory, the script exits with an error.
    This check prevents the script from proceeding with invalid directory input.
    """
    if not os.path.exists(directory):
        error_message("Directory '{}' does not exist.".format(directory))
    if not os.path.isdir(directory):
        error_message("'{}' is not a directory.".format(directory))

def parse_du_output(du_output, directory):
    """
    Parse the output of the 'du' command to find the size of the specified top directory.
    Splits each line of the output by tab character and compares it with the target directory.
    Returns the size of the directory if found, otherwise returns "0B".
    """
    for line in du_output.split('\n'):
        if line:
            size, path = line.split('\t')
            if path.rstrip('/') == directory.rstrip('/'):
                return size.strip()  # Return the size as a string
    return "0B"  # Default return value if not found

def run_custom_du(maxdepth, directory, include_hidden):
    """
    Run a custom 'du' command using 'find' and 'du' with the specified maxdepth.
    Constructs the find command based on the given parameters.
    If include_hidden is False, hidden directories (starting with '.') are excluded.
    """
    check_directory(directory)

    find_command = ['find', directory, '-type', 'd', '-maxdepth', maxdepth]

    # Exclude hidden directories unless include_hidden is True
    if not include_hidden:
        find_command.extend(['!', '-name', '.*'])

    find_command.extend(['-exec', 'du', '-h', '-d', '0', '{}', ';'])

    result = subprocess.check_output(find_command).decode('utf-8')
    print(result)
    total_size = parse_du_output(result, directory)
    print("[INFO] Total: " + total_size)

def main():
    """
    Main function to parse command line arguments and invoke the disk usage report.
    Checks if the script is running on macOS and if the required commands are available.
    Uses argparse for command line argument parsing.
    Includes options for maximum depth and whether to include hidden directories.
    """
    # Check if the script is running on macOS
    if platform.system() != 'Darwin':
        print("[ERROR] This script is intended for use on macOS only.", file=sys.stderr)
        sys.exit(1)

    # Check if the required commands 'find' and 'du' exist
    if not command_exists('find') or not command_exists('du'):
        print("[ERROR] Required commands 'find' or 'du' are not available.", file=sys.stderr)
        sys.exit(2)

    parser = argparse.ArgumentParser(
        description='Simplified Disk Usage Reporting for macOS')
    parser.add_argument('maxdepth', type=int,
                        help='Maximum depth for reporting disk usage')
    parser.add_argument('directory', type=str,
                        help='Directory to report disk usage')
    parser.add_argument('-a', '--all', action='store_true',
                        help='Include hidden directories in the report')

    args = parser.parse_args()

    # Execute the custom disk usage reporting function with provided arguments
    run_custom_du(str(args.maxdepth), args.directory, args.all)

    return 0


# Main execution
if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    if sys.version_info < (3, 2):
        print("[ERROR] This script requires Python 3.2 or later.", file=sys.stderr)
        sys.exit(9)

    sys.exit(main())

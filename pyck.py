#!/usr/bin/env python

########################################################################
# pyck.py: Comprehensive Python Code Formatter and Linter
#
#  Description:
#  This script performs code style checks, auto-formatting, and removal
#  of unused imports for Python files. It uses flake8 for linting,
#  autopep8 for auto-formatting, autoflake for removing unused
#  imports, and isort for organizing imports. The script can operate in
#  dry-run mode to display potential changes without modifying files,
#  and in auto-fix mode to apply changes. It supports multiple files and
#  directories, including wildcard usage.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.6 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v2.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v2.4 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v2.3 2024-01-28
#       Replaced shutil.which with a custom which function to ensure compatibility
#       with Python versions prior to 3.3.
#  v2.2 2024-01-20
#       Refactored to include a main function and separate argument parser setup function.
#  v2.1 2024-01-18
#       Added isort integration for organizing imports.
#       Fixed TypeError in run_command function by decoding stdout to string.
#  v2.0 2024-01-13
#       Ported from shell script (pyck.sh) to Python (pyck.py) for enhanced
#       portability and functionality.
#       Integrated functionality of autopyck.sh, including dry-run mode.
#       Added support for multiple files and directories, including wildcard usage.
#  v1.4 2024-01-07
#       Updated command existence and execution permission checks
#       using a common function for enhanced reliability and maintainability.
#  v1.3 2023-12-20
#       Replaced 'which' with 'command -v' for command existence check.
#  v1.2 2023-12-07
#       Removed dependency on specific Python path.
#  v1.1 2023-12-06
#       Refactored for clarity, added detailed comments, and documentation.
#  v1.0 2014-08-12
#       Initial release.
#
#  Usage:
#  Without -i (Dry-run mode):
#      pyck.py [file(s) or directory(ies)]
#  Example:
#      pyck.py ./my_python_project *.py
#  This mode shows which files would be formatted, cleaned, and have imports organized,
#  without making changes.
#
#  With -i (Actual formatting mode):
#      pyck.py -i [file(s) or directory(ies)]
#  Example:
#      pyck.py -i ./my_python_project *.py
#  This mode actually formats, cleans, and organizes imports in the Python files
#  in the specified files or directories.
#
#  Requirements:
#  - Python Version: 3.2 or later
#  - Dependencies: autopep8, flake8, autoflake, isort
#
########################################################################

import argparse
import glob
import os
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

def setup_argument_parser():
    """ Initialize and return an argument parser for command-line options. """
    parser = argparse.ArgumentParser(
        description="Python Code Formatter and Linter")
    parser.add_argument("paths", nargs='+', type=str,
                        help="Directories or files to format and lint")
    parser.add_argument("-i", "--auto-fix",
                        action="store_true", help="Auto-fix code issues")
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

def format_imports(file_path):
    """ Format and organize imports in a Python file using 'isort'. """
    command = "isort {}".format(file_path)
    subprocess.Popen(command, shell=True).wait()

def dry_run_formatting(paths, ignore_errors):
    """ Perform a dry run to show which files would be formatted and cleaned without making actual changes. """
    for path in paths:
        print(
            "[INFO] DRY RUN: No files will be modified for '{}'. Use -i to auto-fix.".format(path))
        run_command("flake8 --ignore={} {}".format(ignore_errors,
                    path), show_files="Would format:")
        run_command("autoflake --imports=django,requests,urllib3 --check {}".format(path),
                    show_files="Would clean:")
        run_command("isort --check-only {}".format(path),
                    show_files="Would sort imports in:")

def execute_formatting(paths, ignore_errors):
    """ Execute auto-formatting and cleaning on specified Python files or directories. """
    for path in paths:
        actual_path = path[0] if isinstance(path, list) else path

        if os.path.isdir(actual_path):
            for root, dirs, files in os.walk(actual_path):
                for name in files:
                    if name.endswith('.py'):
                        file_path = os.path.join(root, name)
                        format_file(file_path, ignore_errors)
        elif os.path.isfile(actual_path):
            format_file(actual_path, ignore_errors)
        else:
            print("[ERROR] The specified path '{}' is neither a file nor a directory.".format(
                actual_path), file=sys.stderr)

def format_file(file_path, ignore_errors):
    """ Format a single Python file by cleaning up imports, and applying 'autopep8' and 'isort'. """
    command = "autoflake --imports=django,requests,urllib3 -i {}".format(
        file_path)
    subprocess.Popen(command, shell=True).wait()
    command = "autopep8 --ignore={} -v -i {}".format(ignore_errors, file_path)
    subprocess.Popen(command, shell=True).wait()
    format_imports(file_path)

def run_command(command, show_files=None):
    """ Execute a shell command and optionally display formatted files. """
    process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
    stdout, _ = process.communicate()
    if isinstance(stdout, bytes):
        stdout = stdout.decode('utf-8')
    if process.returncode != 0 and show_files:
        for line in stdout.split('\n'):
            if line:
                print("{} {}".format(show_files, line))

def main():
    """ Parse command-line arguments and perform formatting or dry-run based on the input. """
    parser = setup_argument_parser()
    args = parser.parse_args()

    expanded_paths = []
    for path in args.paths:
        expanded_paths.extend(glob.glob(path) or [path])

    ignore_errors = "E302,E402,E501"
    check_command("autopep8")
    check_command("flake8")
    check_command("autoflake")
    check_command("isort")

    if args.auto_fix:
        execute_formatting(expanded_paths, ignore_errors)
    else:
        dry_run_formatting(expanded_paths, ignore_errors)

    return 0


if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    if sys.version_info < (3, 2):
        print("[ERROR] This script requires Python 3.2 or later.", file=sys.stderr)
        sys.exit(9)

    sys.exit(main())

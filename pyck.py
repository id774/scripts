#!/usr/bin/env python

########################################################################
# pyck.py: Comprehensive Python Code Formatter and Linter
#
#  Description:
#  This script performs code style checks, auto-formatting, and removal
#  of unused imports for Python files. It uses flake8 for linting,
#  autopep8 for auto-formatting, and autoflake for removing unused
#  imports. The script can operate in dry-run mode to display potential
#  changes without modifying files, and in auto-fix mode to apply changes.
#  It now supports multiple files and directories, including wildcard usage.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
#    ./pyck.py [file(s) or directory(ies)]
#    Example: ./pyck.py ./my_python_project *.py
#    This mode shows which files would be formatted and cleaned, without making changes.
#
#  With -i (Actual formatting mode):
#    ./pyck.py -i [file(s) or directory(ies)]
#    Example: ./pyck.py -i ./my_python_project *.py
#    This mode actually formats and cleans the Python files in the specified files or directories.
#
########################################################################

import argparse
import subprocess
import os
import sys
import shutil
import glob

def check_command(cmd):
    if not shutil.which(cmd):
        print("Error: Command '{}' is not installed. Please install {} and try again.".format(
            cmd, cmd))
        sys.exit(127)
    elif not os.access(shutil.which(cmd), os.X_OK):
        print("Error: Command '{}' is not executable. Please check the permissions.".format(cmd))
        sys.exit(126)

def dry_run_formatting(paths, ignore_errors):
    for path in paths:
        print(
            "Dry run: No files will be modified for '{}'. Use -i to auto-fix.".format(path))
        run_command("flake8 --ignore={} {}".format(ignore_errors,
                    path), show_files="Would format:")
        run_command("autoflake --imports=django,requests,urllib3 --check {}".format(path),
                    show_files="Would clean:")

def execute_formatting(paths, ignore_errors):
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
            print("Error: The specified path '{}' is neither a file nor a directory.".format(
                actual_path))

def format_file(file_path, ignore_errors):
    command = "autoflake --imports=django,requests,urllib3 -i {}".format(
        file_path)
    subprocess.Popen(command, shell=True).wait()
    command = "autopep8 --ignore={} -v -i {}".format(ignore_errors, file_path)
    subprocess.Popen(command, shell=True).wait()

def run_command(command, show_files=None):
    process = subprocess.Popen(command, shell=True, stdout=subprocess.PIPE)
    stdout, _ = process.communicate()
    if process.returncode != 0 and show_files:
        for line in stdout.split('\n'):
            if line:
                print("{} {}".format(show_files, line))

def main():
    parser = argparse.ArgumentParser(
        description="Python Code Formatter and Linter")
    parser.add_argument("paths", nargs='+', type=str,
                        help="Directories or files to format and lint")
    parser.add_argument("-i", "--auto-fix",
                        action="store_true", help="Auto-fix code issues")
    args = parser.parse_args()

    expanded_paths = []
    for path in args.paths:
        expanded_paths.extend(glob.glob(path) or [path])

    ignore_errors = "E302,E402"
    check_command("autopep8")
    check_command("flake8")
    check_command("autoflake")

    if args.auto_fix:
        execute_formatting(expanded_paths, ignore_errors)
    else:
        dry_run_formatting(expanded_paths, ignore_errors)


if __name__ == "__main__":
    main()

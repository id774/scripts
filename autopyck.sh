#!/bin/sh

########################################################################
# autopyck.sh: Auto-format and Lint Python Code
#
#  Description:
#  This script uses autopep8 for auto-formatting and flake8 for linting
#  Python code. It's designed to improve code quality and adherence to
#  PEP8 standards. The script operates in either dry-run mode or actual
#  formatting mode, controlled by the -i option.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.4 2024-01-07
#       Updated command existence and execution permission checks
#       using a common function for enhanced reliability and maintainability.
#  v1.3 2023-12-20
#       Replaced 'which' with 'command -v' for command existence check.
#  v1.2 2023-12-07
#       Added dry-run functionality and -i option for actual formatting.
#       Updated documentation for clarity.
#       Removed dependency on specific Python path.
#  v1.1 2023-12-06
#       Refactored for clarity, added detailed comments, and documentation.
#  v1.0 2014-08-14
#       Initial release.
#
#  Usage:
#  Without -i (Dry-run mode):
#    ./autopyck.sh [directory]
#    Example: ./autopyck.sh ./my_python_project
#    This mode shows which files would be formatted, without making changes.
#
#  With -i (Actual formatting mode):
#    ./autopyck.sh -i [directory]
#    Example: ./autopyck.sh -i ./my_python_project
#    This mode actually formats the Python files in the specified directory.
#
########################################################################

IGNORE_ERRORS=E302,E402
AUTO_FIX=false

check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again."
            exit 9
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions."
            exit 9
        fi
    done
}

# Check for -i option to auto-fix
while getopts ":i" opt; do
  case $opt in
    i)
      AUTO_FIX=true
      ;;
    \?)
      echo "Invalid option: -$OPTARG" >&2
      exit 1
      ;;
  esac
done

shift $((OPTIND-1))

# Check if a directory is provided
if [ -z "$1" ]; then
    echo "Usage: $0 [directory]"
    echo "Add -i to auto-fix code issues."
    echo "Example: $0 -i ./my_python_project"
    exit 1
fi

DIR=$1

dry_run_formatting() {
    echo "Dry run: No files will be modified. Use -i to auto-fix."
    flake8 --ignore=$IGNORE_ERRORS "$DIR" | cut -d: -f 1 | sort | uniq | xargs -n 1 echo "Would format:"
}

execute_formatting() {
    echo "Auto-fixing code issues in directory: $DIR"
    flake8 --ignore=$IGNORE_ERRORS $DIR | cut -d: -f 1 | sort | uniq | xargs autopep8 --ignore=$IGNORE_ERRORS -v -i
}

main() {
    check_commands autopep8 flake8

    if [ "$AUTO_FIX" = true ]; then
        execute_formatting
    else
        dry_run_formatting
    fi
}

main "$@"

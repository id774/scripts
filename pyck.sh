#!/bin/sh

########################################################################
# pyck.sh: Python Code Check and Auto-Fix
#
#  Description:
#  This script performs code style checks and auto-fixes for Python files.
#  It uses flake8 for linting, autopep8 for auto-formatting, and autoflake
#  for removing unused imports.
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
#       Removed dependency on specific Python path.
#  v1.1 2023-12-06
#       Refactored for clarity, added detailed comments, and documentation.
#  v1.0 2014-08-12
#       Initial release.
#
#  Usage:
#  ./pyck.sh [options] [files...]
#  Options:
#    -i: Auto-fix code issues (using autopep8 and autoflake)
#
########################################################################

IGNORE_ERRORS=E302,E402

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

run_check() {
    flake8 --ignore=$IGNORE_ERRORS "$@"
}

autofix() {
    shift
    autopep8 --ignore=$IGNORE_ERRORS -v -i "$@"
    autoflake --imports=django,requests,urllib3 -i "$@"
}

main() {
    check_commands autopep8 flake8 autoflake

    if [ "$1" = "-i" ]; then
        autofix "$@"
    else
        run_check "$@"
    fi
    exit 0
}

main "$@"

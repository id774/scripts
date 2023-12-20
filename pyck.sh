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

run_check() {
    flake8 --ignore=$IGNORE_ERRORS "$@"
}

autofix() {
    shift
    autopep8 --ignore=$IGNORE_ERRORS -v -i "$@"
    autoflake --imports=django,requests,urllib3 -i "$@"
}

main() {
    command -v autopep8 > /dev/null || { echo "autopep8 not found"; exit 1; }
    command -v flake8 > /dev/null || { echo "flake8 not found"; exit 1; }
    command -v autoflake > /dev/null || { echo "autoflake not found"; exit 1; }

    if [ "$1" = "-i" ]; then
        autofix "$@"
    else
        run_check "$@"
    fi
    exit 0
}

main "$@"

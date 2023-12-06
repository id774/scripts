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
#  v1.0 2014-08-12
#       Initial release.
#
#  Usage:
#  ./pyck.sh [options] [files...]
#  Options:
#    -i: Auto-fix code issues (using autopep8 and autoflake)
#
########################################################################

setup_environment() {
    PYTHON_PATH=/opt/python/current
    IGNORE_ERRORS=E302,E402
}

run_check() {
    $PYTHON_PATH/bin/flake8 --ignore=$IGNORE_ERRORS "$@"
}

autofix() {
    shift
    $PYTHON_PATH/bin/autopep8 --ignore=$IGNORE_ERRORS -v -i "$@"
    $PYTHON_PATH/bin/autoflake --imports=django,requests,urllib3 -i "$@"
}

main() {
    setup_environment "$@"
    which $PYTHON_PATH/bin/autopep8 > /dev/null || { echo "autopep8 not found"; exit 1; }
    which $PYTHON_PATH/bin/flake8 > /dev/null || { echo "flake8 not found"; exit 1; }
    which $PYTHON_PATH/bin/autoflake > /dev/null || { echo "autoflake not found"; exit 1; }

    if [ "$1" = "-i" ]; then
        autofix "$@"
    else
        run_check "$@"
    fi
    exit 0
}

main "$@"


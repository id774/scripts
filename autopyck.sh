#!/bin/sh

########################################################################
# autopyck.sh: Auto-format and Lint Python Code
#
#  Description:
#  This script uses autopep8 for auto-formatting and flake8 for linting
#  Python code. It's designed to improve code quality and adherence to
#  PEP8 standards. The script allows specification of directories and
#  customization of ignored flake8 errors.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-06
#       Refactored for clarity, added detailed comments, and documentation.
#  v1.0 2014-08-14
#       Initial release.
#
#  Usage:
#  ./autopyck.sh [directory]
#  Example: ./autopyck.sh ./my_python_project
#
########################################################################

setup_environment() {
    PYTHON_PATH=/opt/python/current
    IGNORE_ERRORS=E302,E402
    DIR=${1:-"."}
}

main() {
    setup_environment "$@"
    which $PYTHON_PATH/bin/autopep8 > /dev/null || { echo "autopep8 not found"; exit 1; }
    which $PYTHON_PATH/bin/flake8 > /dev/null || { echo "flake8 not found"; exit 1; }

    # Lint and auto-format the specified directory
    $PYTHON_PATH/bin/flake8 --ignore=$IGNORE_ERRORS $DIR | cut -d: -f 1 | sort | uniq | xargs $PYTHON_PATH/bin/autopep8 --ignore=$IGNORE_ERRORS -v -i
    exit 0
}

main "$@"


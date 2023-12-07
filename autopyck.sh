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
#  v1.2 2023-12-07
#       Removed dependency on specific Python path.
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

IGNORE_ERRORS=E302,E402
DIR=${1:-"."}

main() {
    which autopep8 > /dev/null || { echo "autopep8 not found"; exit 1; }
    which flake8 > /dev/null || { echo "flake8 not found"; exit 1; }

    # Lint and auto-format the specified directory
    flake8 --ignore=$IGNORE_ERRORS $DIR | cut -d: -f 1 | sort | uniq | xargs autopep8 --ignore=$IGNORE_ERRORS -v -i
    exit 0
}

main "$@"


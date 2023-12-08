#!/bin/bash

########################################################################
# find_fstrings.sh: Find f-strings in Python files
#
# Description:
# This script searches for f-strings in Python files within a specified directory.
#
# Author: id774 (More info: http://id774.net)
# Source Code: https://github.com/id774/scripts
# License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
# Contact: idnanashi@gmail.com
#
# Version History:
#  v1.0 2023-12-08
#       Initial release. Search for f-strings in Python files.
#
# Usage:
# ./find_fstrings.sh [directory]
# If no directory is specified, it searches in the current directory.
#
# Note:
# - This script requires grep to be installed on the system.
# - The script uses regular expression to find lines that might contain f-strings.
#
########################################################################

# Set the target directory
TARGET_DIR=${1:-.}

# Search for f-strings in Python files
grep -n -r --include="*.py" "f['\"]" "$TARGET_DIR"


#!/bin/sh

########################################################################
# find_python_compat_issues.sh: Find Python compatibility issues in Python files
#
# Description:
# This script searches for Python compatibility issues such as f-strings,
# subprocess.DEVNULL usage, and usage of async/await keywords in Python files
# within a specified directory.
#
# Author: id774 (More info: http://id774.net)
# Source Code: https://github.com/id774/scripts
# License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
# Contact: idnanashi@gmail.com
#
# Version History:
#  v1.1 2023-12-17
#       Added search for async/await keyword usage for Python compatibility issues.
#       Extended the script to search for subprocess.DEVNULL usage for Python compatibility issues.
#  v1.0 2023-12-08
#       Initial release. Search for f-strings in Python files.
#
# Usage:
# ./find_python_compat_issues.sh [directory]
# If no directory is specified, it searches in the current directory.
#
# Note:
# - This script requires grep to be installed on the system.
# - The script uses regular expressions to find lines that might contain
#   compatibility issues.
#
########################################################################

# Set the target directory
TARGET_DIR=${1:-.}

# Search for f-strings in Python files
echo "Searching for f-strings in Python files..."
grep -n -r --include="*.py" "f['\"]" "$TARGET_DIR"

# Search for subprocess.DEVNULL usage in Python files
echo "Searching for subprocess.DEVNULL usage in Python files..."
grep -n -r --include="*.py" "subprocess.DEVNULL" "$TARGET_DIR"

# Search for async and await keyword usage in Python files
echo "Searching for async and await keyword usage in Python files..."
grep -n -r --include="*.py" -E "\basync\b|\bawait\b" "$TARGET_DIR"


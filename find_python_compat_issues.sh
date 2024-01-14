#!/bin/sh

########################################################################
# find_python_compat_issues.sh: Find Python compatibility issues in Python files
#
#  Description:
#  This script searches for Python compatibility issues in Python files
#  within a specified directory. It looks for features introduced in Python 3.x,
#  such as f-strings, subprocess.run, subprocess.DEVNULL, async/await keywords,
#  type hints, nonlocal statements, matrix multiplication operators, asyncio library,
#  yield from, extended unpacking, and pathlib module.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2024-01-14
#       Added search for Python 3.x features like type hints, nonlocal statements,
#       matrix multiplication operators, asyncio library, yield from, extended unpacking,
#       pathlib module, and subprocess.run usage.
#  v1.2 2023-12-23
#       Added search for async/await keyword usage and subprocess.DEVNULL usage.
#  v1.1 2023-12-17
#       Enhanced script to support POSIX compliance and cross-system compatibility.
#  v1.0 2023-12-08
#       Initial release. Search for f-strings in Python files.
#
#  Usage:
#  ./find_python_compat_issues.sh [directory]
#  If no directory is specified, it searches in the current directory.
#
#  Note:
#  - This script requires grep to be installed on the system.
#
########################################################################

# Set the target directory
TARGET_DIR=${1:-.}

echo "Searching for Python 3.x compatibility issues in Python files..."

# Search for f-strings
grep -n -r --include="*.py" "f['\"]" "$TARGET_DIR"

# Search for subprocess.run and subprocess.DEVNULL
grep -n -r --include="*.py" -E "subprocess.run|subprocess.DEVNULL" "$TARGET_DIR"

# Search for async/await keywords
grep -n -r --include="*.py" -E "\basync\b|\bawait\b" "$TARGET_DIR"

# Search for nonlocal keyword usage
echo "Searching for nonlocal keyword usage in Python files..."
grep -n -r --include="*.py" -E "\bnonlocal\b" "$TARGET_DIR"

# Search for decorator syntax usage with common Python 3 decorators
echo "Searching for common Python 3 decorator syntax usage in Python files..."
grep -n -r --include="*.py" -E "^\s*@(staticmethod|classmethod|property|functools\.[a-zA-Z0-9_]+)" "$TARGET_DIR"

# Search for asyncio usage
echo "Searching for asyncio usage in Python files..."
grep -n -r --include="*.py" -E "\basyncio\." "$TARGET_DIR"

# Search for 'yield from' usage
echo "Searching for 'yield from' usage in Python files..."
grep -n -r --include="*.py" -E "\byield from\b" "$TARGET_DIR"

# Search for matrix multiplication operator usage
echo "Searching for matrix multiplication operator usage in Python files..."
grep -n -r --include="*.py" -E "\b[a-zA-Z_][a-zA-Z0-9_]*\s*@\s*[a-zA-Z_][a-zA-Z0-9_]*\b" "$TARGET_DIR" | grep -v "mail"

# Search for pathlib usage
echo "Searching for pathlib usage in Python files..."
grep -n -r --include="*.py" -E "\bpathlib\." "$TARGET_DIR"

# Search for type hints
echo "Searching for type hints in Python files..."
grep -n -r --include="*.py" -E "\bdef\b.*->" "$TARGET_DIR"

echo "Search completed."

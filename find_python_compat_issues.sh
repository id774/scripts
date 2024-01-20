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
#  v1.4 2024-01-20
#       Improved f-strings detection regular expression to accurately identify common patterns.
#  v1.3 2024-01-14
#       Added search for Python 3.x features like type hints, nonlocal statements,
#       matrix multiplication operators, asyncio library, yield from, extended unpacking,
#       pathlib module, and subprocess.run usage.
#       Added check for the existence of 'grep' command before execution.
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

check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again."
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions."
            exit 126
        fi
    done
}

check_commands "grep"

# Set the target directory
TARGET_DIR=${1:-.}

echo "*** Searching for Python 3.x compatibility issues in Python files..."

# Search for f-strings
echo "*** Searching for f-strings..."
grep -n -r --include="*.py" -E "f['\"][^'\"]*\{[^}]*\}[^'\"]*['\"]" "$TARGET_DIR" | grep -v '^#'

# Searching subprocess.run and subprocess.DEVNULL
echo "*** Searching for subprocess.run and subprocess.DEVNULL..."
grep -n -r --include="*.py" -E "subprocess.run|subprocess.DEVNULL" "$TARGET_DIR" | grep -v '^#'

# Searching async/await keywords
echo "*** Searching for async/await keywords..."
grep -n -r --include="*.py" -E "\basync\b|\bawait\b" "$TARGET_DIR" | grep -v '^#'

# Search for nonlocal keyword usage
echo "*** Searching for nonlocal keyword usage in Python files..."
grep -n -r --include="*.py" -E "\bnonlocal\b" "$TARGET_DIR" | grep -v '^#'

# Search for asyncio usage
echo "*** Searching for asyncio usage in Python files..."
grep -n -r --include="*.py" -E "\basyncio\." "$TARGET_DIR" | grep -v '^#'

# Search for 'yield from' usage
echo "*** Searching for 'yield from' usage in Python files..."
grep -n -r --include="*.py" -E "\byield from\b" "$TARGET_DIR" | grep -v '^#'

# Search for matrix multiplication operator usage
echo "*** Searching for matrix multiplication operator usage in Python files..."
grep -n -r --include="*.py" -E "\b[a-zA-Z_][a-zA-Z0-9_]*\s*@\s*[a-zA-Z_][a-zA-Z0-9_]*\b" "$TARGET_DIR" | grep -v "mail" | grep -v '^#'

# Search for pathlib usage
echo "*** Searching for pathlib usage in Python files..."
grep -n -r --include="*.py" -E "\bpathlib\." "$TARGET_DIR" | grep -v '^#'

# Search for type hints
echo "*** Searching for type hints in Python files..."
grep -n -r --include="*.py" -E "\bdef\b.*->" "$TARGET_DIR" | grep -v '^#'

echo "Search completed."

#!/usr/bin/env python

########################################################################
# check_py_compat.py: Check Python compatibility issues in Python files
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
#  v2.1 2024-01-28
#       Added detection for shutil.which usage to enhance compatibility checks.
#  v2.0 2024-01-21
#       Ported script from shell to Python. Removed usage of Python 3.x specific features
#       for compatibility checks. Enhanced modularity for better testability.
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
# Usage:
# ./check_py_compat.py [directory]
# If no directory is specified, it searches in the current directory.
#
########################################################################

import os
import re
import sys


def is_excluded_line(line, script_patterns):
    """ Check if the line should be excluded from search (e.g., comments, certain words, script patterns) """
    return any(line.strip().startswith(comment) or term in line for comment, term in script_patterns)

def search_feature(directory, feature_name, pattern, script_patterns):
    print("*** Searching for " + feature_name + "...")
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith('.py'):
                file_path = os.path.join(root, file)
                with open(file_path, 'r', encoding='utf-8') as f:
                    for i, line in enumerate(f):
                        if re.search(pattern, line) and not is_excluded_line(line, script_patterns):
                            print(file_path + ":" + str(i + 1) + ": " + line.strip())

def main():
    target_dir = sys.argv[1] if len(sys.argv) > 1 else '.'

    # Check if the target directory exists
    if not os.path.isdir(target_dir):
        print(f"Error: Directory '{target_dir}' does not exist.")
        sys.exit(1)

    # Patterns to exclude (comments and patterns used in this script)
    script_patterns = [("#", "mail"), ("r\"", ""), ("r'", "")]

    print("*** Searching for Python 3.x compatibility issues in Python files...")

    # Define patterns for each feature
    features = {
        "f-strings": r"f['\"][^'\"]*\{[^}]*\}[^'\"]*['\"]",
        "subprocess.run and subprocess.DEVNULL": r"subprocess\.run|subprocess\.DEVNULL",
        "async/await keywords": r"\basync\b|\bawait\b",
        "nonlocal keyword": r"\bnonlocal\b",
        "asyncio usage": r"\basyncio\.",
        "yield from usage": r"\byield from\b",
        "matrix multiplication operator": r"\b[a-zA-Z_][a-zA-Z0-9_]*\s*@\s*[a-zA-Z_][a-zA-Z0-9_]*\b",
        "pathlib usage": r"\bpathlib\.",
        "type hints": r"\bdef\b.*->",
        "shutil.which usage": r"\bshutil\.which\b"
    }

    # Perform search for each feature
    for feature, pattern in features.items():
        search_feature(target_dir, feature, pattern, script_patterns)


if __name__ == "__main__":
    main()

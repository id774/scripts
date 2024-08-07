#!/usr/bin/env python

########################################################################
# find_pycompat.py: Check Python compatibility issues in Python files
#
#  Description:
#  This script searches for Python compatibility issues in Python files
#  within a specified directory. It identifies usage of features introduced in Python 3.x,
#  including f-strings, subprocess.run, subprocess.DEVNULL, async/await keywords,
#  type hints, nonlocal statements, matrix multiplication operators, asyncio library,
#  yield from, extended unpacking, pathlib module, and notably, the shutil.which function.
#  The script helps in identifying code segments that may not be compatible with earlier
#  versions of Python, facilitating easier code migration and compatibility assessments.
#  It now also tracks the detected issues and provides feedback on whether the issues
#  are confined to the dummy.py script or present in other scripts as well.
#  If compatibility issues are detected in scripts other than dummy.py, the script
#  exits with a return code of 1 to indicate the presence of such issues.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v3.3 2024-08-07
#       Added -h option to display help. Modified script to show help when no directory is specified.
#  v3.2 2024-03-12
#       Modified the detection pattern for the matrix multiplication operator
#       to require spaces around it.
#       Updated the script to exit with a return code of 1 if compatibility issues
#       are detected in scripts other than dummy.py.
#  v3.1 2024-02-27
#       Enhanced issue tracking to differentiate between issues found in dummy.py and other scripts.
#  v3.0 2024-02-11
#       Fixed an issue where valid search results were not being displayed.
#       The script and its associated test file are now excluded from search results.
#       Updated comments to English for better clarity.
#  v2.3 2024-01-31
#       Renamed script from 'check_py_compat.py' to 'find_pycompat.py'
#       to improve clarity and ease of use.
#  v2.2 2024-01-30
#       Enhanced script compatibility with older Python versions.
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
#  Usage:
#  ./find_pycompat.py [directory]
#  If no directory is specified, it searches in the current directory.
#
#  Notes:
#  This script excludes certain lines from the search to avoid false positives. Specifically,
#  lines that are comments (starting with '#') or contain email addresses are excluded.
#  This is to prevent the inclusion of non-code elements like email addresses, which may
#  contain patterns resembling Python 3.x features but are unrelated to code functionality.
#
########################################################################

import os
import re
import sys

detected_issues = []  # List to store detected issues

def is_excluded_line(line):
    """ Check if the line should be excluded from search (e.g., comments). """
    # Check if the line is a comment
    email_pattern = re.compile(r'[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}')
    if email_pattern.search(line):
        return True  # If an email address is found, exclude this line
    return line.strip().startswith("#")

def search_feature(directory, feature_name, pattern):
    print("*** Searching for " + feature_name + "...")

    # Get the base name of the current script without extension
    script_base_name = os.path.basename(__file__).replace('.py', '')
    # Define the pattern for test files related to this script
    test_file_pattern = script_base_name + '_test.py'

    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith('.py'):
                # Skip the script itself and its test file
                if file == os.path.basename(__file__) or file == test_file_pattern:
                    continue
                file_path = os.path.join(root, file)
                with open(file_path, 'r', encoding='utf-8') as f:
                    for i, line in enumerate(f):
                        if re.search(pattern, line) and not is_excluded_line(line):
                            print(file_path + ":" + str(i + 1) + ": " + line.strip())
                            detected_issues.append(os.path.basename(file_path))  # Store only the file name, not the entire path

def display_help():
    print("""
Usage: ./find_pycompat.py [options] [directory]

Options:
  -h                Display this help message and exit

If no directory is specified, it displays this help message.
To check the current directory, use:
  ./find_pycompat.py .
""")

def main():
    if len(sys.argv) == 1:
        display_help()
        sys.exit(0)
    elif sys.argv[1] == '-h':
        display_help()
        sys.exit(0)

    target_dir = sys.argv[1]

    # Check if the target directory exists
    if not os.path.isdir(target_dir):
        print("Error: Directory '{}' does not exist.".format(target_dir))
        sys.exit(1)

    print("*** Searching for Python 3.x compatibility issues in Python files...")

    # Define patterns for each feature
    features = {
        "f-strings": r"f['\"][^'\"]*\{[^}]*\}[^'\"]*['\"]",
        "subprocess.run and subprocess.DEVNULL": r"subprocess\.run|subprocess\.DEVNULL",
        "async/await keywords": r"\basync\b|\bawait\b",
        "nonlocal keyword": r"\bnonlocal\b",
        "asyncio usage": r"\basyncio\.",
        "yield from usage": r"\byield from\b",
        "matrix multiplication operator": r"\b[a-zA-Z_][a-zA-Z0-9_]*\s+@\s+[a-zA-Z_][a-zA-Z0-9_]*\b",
        "pathlib usage": r"\bpathlib\.",
        "type hints": r"\bdef\b.*->",
        "shutil.which usage": r"\bshutil\.which\b"
    }

    # Perform search for each feature
    for feature, pattern in features.items():
        search_feature(target_dir, feature, pattern)

    unique_issues = set(detected_issues)  # Create a set of unique entries from detected issues

    # Check if the set of unique issues contains only 'dummy.py'
    if not detected_issues:
        print("No Python 3.x compatibility issues found.")
    elif unique_issues == {'dummy.py'}:
        print("Only dummy.py was detected with Python 3.x features, which is expected. No compatibility issues found in other scripts.")
    else:
        print("Compatibility issues detected in scripts other than dummy.py. Please review the findings.")
        sys.exit(1)  # Exit with a status code of 1 to indicate an error.


if __name__ == "__main__":
    main()

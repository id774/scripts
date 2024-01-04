#!/bin/sh

########################################################################
# tree.sh: Display Directory Tree
#
#  Description:
#  This script displays the directory tree of the specified directory,
#  or the current directory if no directory is specified.
#  It uses 'find', 'sort', and 'sed' commands to format the tree structure.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2024-01-04
#       Added functionality to accept a directory as an argument. If no
#       argument is provided, the script displays the tree of the current
#       directory. Added error handling for non-existent directories.
#  v1.1 2023-12-23
#       Refactored for POSIX compliance. Replaced Bash-specific syntax
#       with POSIX standard commands and structures. Enhanced portability
#       and compatibility across different UNIX-like systems.
#  v1.0 2014-10-22
#       Initial release.
#
#  Usage:
#  ./tree.sh [directory]
#  Run the script without arguments to display the tree of the current
#  directory, or with a directory path to display the tree of that directory.
#
########################################################################

# Check if an argument is given
if [ $# -eq 0 ]; then
    directory="."
else
    directory="$1"
    # Check if the directory exists
    if [ ! -d "$directory" ]; then
        echo "Error: Directory '$directory' does not exist."
        exit 1
    fi
fi

# Display the specified directory
echo "$directory"
cd "$directory" || exit

# Generate and display the directory tree
find . | sort | sed '1d;s/^\.//;s/\/\([^/]*\)$/|--\1/;s/\/[^/|]*/|  /g'


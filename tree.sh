#!/bin/sh

########################################################################
# tree.sh: Display Directory Tree
#
#  Description:
#  This script displays the directory tree of the specified directory,
#  or the current directory if no directory is specified. Hidden directories
#  (those starting with a dot) are excluded by default. An option can be
#  provided to include these hidden directories in the tree.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2024-01-07
#       Added an option to include hidden directories in the tree display.
#       Updated command existence and execution permission checks
#       using a common function for enhanced reliability and maintainability.
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
#  ./tree.sh [directory] [-a]
#  Run the script without arguments to display the tree of the current
#  directory, with a directory path to display the tree of that directory,
#  or with '-a' option to include hidden directories.
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

# Check if required commands are available and executable
check_commands find sort sed

# Check for hidden directory display option
show_hidden=false
if [ "$#" -gt 0 ] && [ "$1" = "-a" ]; then
  show_hidden=true
  shift
fi

# Check if an argument is given for directory
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
if [ "$show_hidden" = true ]; then
  find . -print | sort | sed '1d;s/^\.//;s/\/\([^/]*\)$/|--\1/;s/\/[^/|]*/|  /g'
else
  find . -not -path '*/\.*' -print | sort | sed '1d;s/^\.//;s/\/\([^/]*\)$/|--\1/;s/\/[^/|]*/|  /g'
fi


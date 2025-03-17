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
#  v1.5 2025-03-17
#       Encapsulated all logic in functions and introduced main function.
#  v1.4 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
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

# Function to check if required commands exist
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to determine the directory and option for hidden files
parse_arguments() {
    show_hidden=false

    if [ "$#" -gt 0 ] && [ "$1" = "-a" ]; then
        show_hidden=true
        shift
    fi

    if [ $# -eq 0 ]; then
        directory="."
    else
        directory="$1"
        if [ ! -d "$directory" ]; then
            echo "Error: Directory '$directory' does not exist." >&2
            exit 1
        fi
    fi
}

# Function to display the directory tree
display_tree() {
    echo "$directory"
    cd "$directory" || exit 1

    if [ "$show_hidden" = true ]; then
        find . -print | sort | sed '1d;s/^\.//;s/\/\([^/]*\)$/|--\1/;s/\/[^/|]*/|  /g'
    else
        find . -not -path '*/\.*' -print | sort | sed '1d;s/^\.//;s/\/\([^/]*\)$/|--\1/;s/\/[^/|]*/|  /g'
    fi
}

# Main function to execute the script
main() {
    check_commands find sort sed
    parse_arguments "$@"
    display_tree
}

# Execute main function
main "$@"

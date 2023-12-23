#!/bin/sh

########################################################################
# tree.sh: Display Directory Tree
#
#  Description:
#  This script displays the directory tree of the current directory.
#  It uses 'find', 'sort', and 'sed' commands to format the tree structure.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-23
#       Refactored for POSIX compliance. Replaced Bash-specific syntax
#       with POSIX standard commands and structures. Enhanced portability
#       and compatibility across different UNIX-like systems.
#  v1.0 2014-10-22
#       Initial release.
#
#  Usage:
#  ./tree.sh
#  Run the script in the directory you want to display the tree for.
#
########################################################################

# Display the current directory
pwd

# Generate and display the directory tree
find . | sort | sed '1d;s/^\.//;s/\/\([^/]*\)$/|--\1/;s/\/[^/|]*/|  /g'


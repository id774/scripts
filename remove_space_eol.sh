#!/bin/sh

########################################################################
# remove_space_eol.sh: End-of-Line Whitespace Removal Script
#
#  Description:
#  This script removes trailing whitespace from each line in the specified files.
#  It's useful for cleaning up source code files and other text files.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-05
#       Refactored for better error handling and added comments.
#  v1.0 2008-08-22
#       Initial release. Removes trailing whitespace from files.
#
#  Usage:
#  find [directory...] -type f -name "*" -exec /path/to/remove_space_eol.sh {} \;
#
########################################################################

# Process each file passed as an argument
while [ $# -gt 0 ]
do
  # Create a temporary file
  if mv "$1" "$1.tmp"; then
    # Remove trailing whitespace and overwrite the original file
    if sed -e 's/[[:blank:]]*$//' "$1.tmp" > "$1"; then
      echo "Removed trailing whitespace from '$1'"
    else
      echo "Error processing '$1'"
    fi
    # Remove the temporary file
    rm "$1.tmp"
  else
    echo "Error moving '$1' to temporary file"
  fi

  # Move to the next file
  shift
done


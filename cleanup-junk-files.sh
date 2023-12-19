#!/bin/zsh

########################################################################
# cleanup-junk-files.sh: Clean Up Junk Files
#
#  Description:
#  This script removes common junk files from a specified directory.
#  It targets files like .DS_Store, ._* AppleDouble files, temporary
#  Unix files ending with '.un~', and __pycache__ directories.
#
#  Notes:
#  - This script will recursively delete the specified junk files in the target directory.
#  - Use with caution and ensure that the target directory is correct.
#  - It is advisable to perform a dry run or backup important files before execution.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2023-12-20
#       Added feature to remove __pycache__ directories.
#  v1.1 2023-12-06
#       Refactored for improved readability, added detailed comments and notes.
#  v1.0 2016-08-05
#       Initial release.
#
#  Usage:
#  ./cleanup-junk-files.sh <target_directory>
#  Example: ./cleanup-junk-files.sh /path/to/directory
#
########################################################################

# Check if a directory is provided
if [ -z "$1" ]; then
  echo "Error: No target directory provided."
  echo "Usage: $0 <target_directory>"
  exit 1
fi

# Remove common junk files from the specified directory
echo "Cleaning up junk files in $1..."
rm -vf $1/**/._*
rm -vf $1/**/.DS_Store
rm -vf $1/**/.*.un~
rm -vrf $1/**/__pycache__

echo "Cleanup completed."


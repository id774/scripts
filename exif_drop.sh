#!/bin/sh

########################################################################
# exif_drop.sh: Remove EXIF Data from Images with GPS Information
#
#  Description:
#  This script searches for image files with GPS information in their
#  EXIF data and removes this data using jhead for privacy reasons.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2024-08-11
#       Added input validation to ensure directory is provided as an argument.
#       Display help message if no arguments are provided.
#  v1.1 2023-12-06
#       Refactored for improved readability and added detailed comments.
#  v1.0 2009-10-05
#       Initial release.
#
#  Usage:
#  ./exif_drop.sh <directory_path>
#
########################################################################

FIND=/usr/bin/find
EXIFTOOL=/usr/bin/exiftool
JHEAD=/usr/bin/jhead

# Function to display help message
display_help() {
  cat << EOF
Usage: $0 <directory_path>

Description:
  This script searches for image files with GPS information in their EXIF data
  and removes this data using jhead for privacy reasons.

Options:
  -h   Display this help message.

Notes:
  Ensure that the exiftool and jhead utilities are installed before running this script.
EOF
}

# Check if arguments are provided
if [ "$#" -eq 0 ]; then
  display_help
  exit 0
fi

# Process -h option
if [ "$1" = "-h" ]; then
  display_help
  exit 0
fi

# Validate the directory path argument
PATH=$1
if [ ! -d "$PATH" ]; then
  echo "Error: '$PATH' is not a valid directory."
  exit 1
fi

# Ensure the required tools are installed
[ -x "$EXIFTOOL" ] || { echo "exiftool not found"; exit 1; }
[ -x "$JHEAD" ] || { echo "jhead not found"; exit 1; }

# Process image files with GPS information
$FIND "$PATH" -type f | while read FILE
do
  if [ -n "$($EXIFTOOL -gps:GPSLatitude "$FILE")" ]; then
    $JHEAD -purejpg "$FILE"
    echo "Processed $FILE"
  fi
done

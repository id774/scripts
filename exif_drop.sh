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
PATH=$1

# Ensure the required tools are installed
[ -x "$EXIFTOOL" ] || { echo "exiftool not found"; exit 1; }
[ -x "$JHEAD" ] || { echo "jhead not found"; exit 1; }

# Process image files with GPS information
$FIND $PATH -type f | while read FILE
do
  if [ -n "$($EXIFTOOL -gps:GPSLatitude "$FILE")" ]; then
    $JHEAD -purejpg "$FILE"
    echo "Processed $FILE"
  fi
done


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
#  v1.4 2025-03-17
#       Encapsulated all logic into functions and introduced main function.
#  v1.3 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
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

FIND=find
EXIFTOOL=exiftool
JHEAD=jhead

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

# Function to check required commands
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

# Function to validate directory
check_directory() {
    if [ ! -d "$1" ]; then
        echo "Error: '$1' is not a valid directory." >&2
        exit 1
    fi
}

# Function to process images and remove EXIF GPS data
process_images() {
    dir="$1"
    $FIND "$dir" -type f | while read file
    do
        if [ -n "$($EXIFTOOL -gps:GPSLatitude "$file")" ]; then
            $JHEAD -purejpg "$file"
            echo "Processed: $file"
        fi
    done
}

# Main function to execute the script
main() {
    if [ "$#" -eq 0 ]; then
        display_help
        exit 0
    fi

    if [ "$1" = "-h" ]; then
        display_help
        exit 0
    fi

    directory="$1"

    check_commands $EXIFTOOL $JHEAD $FIND
    check_directory "$directory"
    process_images "$directory"
}

# Execute main function
main "$@"

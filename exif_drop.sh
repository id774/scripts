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
#  v1.5 2025-03-22
#       Unify usage information by extracting help text from header comments.
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
#      ./exif_drop.sh <directory_path>
#
#  Options:
#      -h   Display this help message.
#
#  Notes:
#  - Ensure that the exiftool and jhead utilities are installed before running this script.
#
########################################################################

FIND=find
EXIFTOOL=exiftool
JHEAD=jhead

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
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
    case "$1" in
        -h|--help) usage ;;
    esac

    if [ "$#" -eq 0 ]; then
        usage
    fi

    directory="$1"

    check_commands $EXIFTOOL $JHEAD $FIND
    check_directory "$directory"
    process_images "$directory"
}

# Execute main function
main "$@"

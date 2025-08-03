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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
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
#  Version History:
#  v1.8 2025-08-03
#       Improve file safety with null-terminated find output.
#       Suppress exiftool stderr output on missing tags.
#       Add validation for single argument constraint.
#  v1.7 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.6 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
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
########################################################################

FIND=find
EXIFTOOL=exiftool
JHEAD=jhead

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if required commands are available and executable
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Validate directory
check_directory() {
    if [ ! -d "$1" ]; then
        echo "[ERROR] '$1' is not a valid directory." >&2
        exit 1
    fi
}

# Process images and remove EXIF GPS data
process_images() {
    dir="$1"

    if [ "$#" -ne 1 ]; then
        echo "[ERROR] Please specify exactly one directory." >&2
        usage
    fi

    $FIND "$dir" -type f -print0 | while IFS= read -r -d '' file
    do
        if [ -n "$($EXIFTOOL -gps:GPSLatitude "$file" 2>/dev/null)" ]; then
            $JHEAD -purejpg "$file"
            echo "[INFO] Processed: $file"
        else
            echo "[INFO] Skipped: $file"
        fi
    done
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    if [ "$#" -eq 0 ]; then
        usage
    fi

    directory="$1"

    check_commands $EXIFTOOL $JHEAD $FIND
    check_directory "$directory"
    process_images "$directory"
    return 0
}

# Execute main function
main "$@"
exit $?

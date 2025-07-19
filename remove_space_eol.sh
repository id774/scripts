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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./find [directory...] -type f -name "*" -exec /path/to/remove_space_eol.sh {} \;
#
#  Version History:
#  v1.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.3 2025-03-17
#       Encapsulated all logic in functions and introduced main function.
#  v1.2 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#       Added command existence check for sed and mv.
#  v1.1 2023-12-05
#       Refactored for better error handling and added comments.
#  v1.0 2008-08-22
#       Initial release. Removes trailing whitespace from files.
#
########################################################################

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

# Remove trailing whitespace from a file
process_file() {
    file="$1"

    if mv "$file" "$file.tmp"; then
        if sed -e 's/[[:blank:]]*$//' "$file.tmp" > "$file"; then
            echo "[INFO] Removed trailing whitespace from '$file'."
        else
            echo "[ERROR] Error processing '$file'." >&2
        fi
        rm "$file.tmp"
    else
        echo "[ERROR] Error moving '$file' to temporary file." >&2
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_commands sed mv

    while [ $# -gt 0 ]; do
        process_file "$1"
        shift
    done
    return 0
}

# Execute main function
main "$@"
exit $?

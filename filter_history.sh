#!/bin/sh

########################################################################
# filter_history.sh: Remove specific entries from Zsh history file
#
#  Description:
#  This script removes lines containing a specified pattern from the Zsh
#  history file. It creates a backup before modification and ensures
#  safe execution with error handling.
#
#  Features:
#  - Creates a backup before modifying the history file.
#  - Ensures TMP environment variable is set before proceeding.
#  - Uses grep to filter out lines containing the specified pattern.
#  - Provides error handling for file operations.
#  - Displays the number of removed entries.
#  - Checks for required commands before execution.
#  - Displays a diff of the changes made to the history file.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.2 2025-03-16
#       Encapsulated all logic in functions and introduced main function.
#  v1.1 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.0 2025-02-25
#       Initial release with backup creation and error handling.
#
#  Usage:
#  ./filter_history.sh <pattern>
#  <pattern>: The string to remove from the history file (partial match).
#
########################################################################

set -e  # Exit immediately if a command exits with a non-zero status

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

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    # Required commands
    check_commands grep cp mv basename diff

    HISTORY_FILE="$HOME/.zsh_history"

    # Check if TMP is defined
    if [ -z "$TMP" ]; then
        echo "Error: TMP environment variable is not set." >&2
        exit 1
    fi

    BACKUP_FILE="$TMP/$(basename "$HISTORY_FILE").bak.$(date +%Y%m%d%H%M%S)"

    # Check if an argument is provided
    if [ "$#" -ne 1 ]; then
        echo "Usage: $0 <string>"
        exit 0
    fi

    PATTERN="$1"

    # Create a backup of the history file
    cp "$HISTORY_FILE" "$BACKUP_FILE"
    echo "Backup created: $BACKUP_FILE"

    # Count the number of matching lines
    MATCH_COUNT=$(grep -c "$PATTERN" "$HISTORY_FILE")

    # Remove lines containing the specified pattern
    if ! grep -v "$PATTERN" "$HISTORY_FILE" > "$HISTORY_FILE.tmp"; then
        echo "Error: Failed to create temporary file." >&2
        exit 1
    fi

    # Overwrite the original history file
    mv "$HISTORY_FILE.tmp" "$HISTORY_FILE"
    echo "Lines containing '$PATTERN' have been removed from $HISTORY_FILE"
    echo "Total removed entries: $MATCH_COUNT"

    # Show the difference between the backup and the modified file
    echo "Displaying changes:"
    diff "$BACKUP_FILE" "$HISTORY_FILE" || true
}

# Execute main function
main "$@"

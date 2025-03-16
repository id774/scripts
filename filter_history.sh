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
#  v1.2 2025-03-16
#       Encapsulated all logic in functions and introduced main function.
#  v1.1 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.0 2025-02-25
#       - Initial release with backup creation and error handling.
#
#  Usage:
#  ./filter_history.sh <pattern>
#  <pattern>: The string to remove from the history file (partial match).
#
########################################################################

set -e  # Exit immediately if a command exits with a non-zero status

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

# Main function
main() {
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

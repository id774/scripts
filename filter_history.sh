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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      filter_history.sh <pattern>
#
#  <pattern>: The string to remove from the history file (partial match).
#
#  Version History:
#  v1.7 2025-12-15
#       Add date to command checks, validate TMP and history file existence, and improve temp file handling.
#  v1.6 2025-08-07
#       Use 'grep -F' for fixed-string pattern matching to avoid unintended regex behavior.
#  v1.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.4 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.3 2025-03-22
#       Unify usage information by extracting help text from header comments.
#       Removed set -e and added explicit error handling.
#       Refactored main logic into separate functions.
#  v1.2 2025-03-16
#       Encapsulated all logic in functions and introduced main function.
#  v1.1 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.0 2025-02-25
#       Initial release with backup creation and error handling.
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

# Check TMP variable and argument count
validate_environment() {
    # Ensure TMP is defined and an argument is provided
    if [ -z "$TMP" ]; then
        echo "[ERROR] TMP environment variable is not set." >&2
        exit 1
    fi
    if [ ! -d "$TMP" ]; then
        echo "[ERROR] TMP directory does not exist: $TMP" >&2
        exit 1
    fi
    if [ -z "$1" ]; then
        echo "[ERROR] No pattern specified." >&2
        exit 1
    fi
    if [ "$#" -ne 1 ]; then
        usage
    fi
}

# Create a backup of the history file
create_backup() {
    # Backup the original history file
    BACKUP_FILE="$TMP/$(basename "$HISTORY_FILE").bak.$(date +%Y%m%d%H%M%S)"
    if ! cp "$HISTORY_FILE" "$BACKUP_FILE"; then
        echo "[ERROR] Failed to create backup file." >&2
        exit 1
    fi
    echo "[INFO] Backup created: $BACKUP_FILE."
}

# Count matching lines in the history file
count_matches() {
    # Count matching lines (fixed string)
    MATCH_COUNT=$(grep -F -c "$PATTERN" "$HISTORY_FILE" 2>/dev/null)

    if [ $? -ne 0 ]; then
        echo "[ERROR] Failed to count matches in history file." >&2
        exit 1
    fi
}

# Filter out matching lines and overwrite the original file
filter_history() {
    # Remove matching lines (fixed string) and replace the original history file
    tmp="$HISTORY_FILE.tmp.$$"
    if ! grep -F -v "$PATTERN" "$HISTORY_FILE" > "$tmp"; then
        echo "[ERROR] Failed to create temporary file." >&2
        exit 1
    fi

    if ! mv "$tmp" "$HISTORY_FILE"; then
        echo "[ERROR] Failed to overwrite the original history file." >&2
        exit 1
    fi

    echo "[INFO] Lines containing '$PATTERN' have been removed from $HISTORY_FILE."
    echo "[INFO] Total removed entries: $MATCH_COUNT."
}

# Show the diff between backup and modified file
show_diff() {
    # Show difference between backup and updated file
    echo "[INFO] Displaying changes:"
    diff "$BACKUP_FILE" "$HISTORY_FILE" || true
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    validate_environment "$@"

    check_commands grep cp mv basename diff date

    HISTORY_FILE="$HOME/.zsh_history"
    PATTERN="$1"
    if [ ! -f "$HISTORY_FILE" ]; then
        echo "[ERROR] History file not found: $HISTORY_FILE" >&2
        exit 1
    fi
    if [ ! -w "$HISTORY_FILE" ]; then
        echo "[ERROR] History file is not writable: $HISTORY_FILE" >&2
        exit 1
    fi

    create_backup
    count_matches

    filter_history
    if [ "${MATCH_COUNT:-0}" -eq 0 ]; then
        echo "[INFO] No matching entries. File unchanged."
    fi

    show_diff
    return 0
}

# Execute main function
main "$@"

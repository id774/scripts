#!/bin/sh

########################################################################
# sd_extract.sh: Extract data from SD cards
#
#  Description:
#  This script synchronizes specified file types from multiple source directories
#  on SD cards to a destination directory on the local machine. It reads source
#  directories, file patterns, the destination directory, and default file permissions
#  from an external configuration file. The script allows overriding the default
#  file permissions via a command-line argument. If a file copy operation fails,
#  the script will skip the problematic file and continue with the next file. At the end
#  of the process, the script reports the files that failed to copy and exits with a
#  non-zero status if any errors occurred.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.6 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.5 2024-08-26
#       Added validation to ensure that the permissions argument is a 3-digit octal number.
#       Updated error handling with a new return code (6) for invalid permissions input.
#       Added a note to restrict permissions argument to a 3-digit octal number.
#  v1.4 2024-05-17
#       Modified to skip files that cannot be copied due to read errors,
#       report them at the end, and exit with a non-zero status if there were any errors.
#  v1.3 2024-02-09
#       Enhanced documentation, added configuration variable checks, improved error
#       handling, and script structure. Introduced a method to check command availability.
#  v1.2 2024-02-01
#       Updated to use a default permissions setting from the configuration file.
#       Users can still override this setting via command-line argument.
#  v1.1 2024-01-30
#       Fixed an issue where the loop processing file lists did not function correctly
#       due to subshell execution in the 'find ... | while read' pipeline.
#       This caused variables set within the loop to not be accessible outside of it.
#       Introduced a flag file method to accurately detect successful file copy operations.
#       This change was made to overcome the limitation of variable scope within subshell execution,
#       ensuring that the script accurately reflects the outcome of file synchronization processes.
#  v1.0 2024-01-29
#       Initial release. Supports synchronization of specified file types from
#       multiple source directories to a local destination directory, only if they exist.
#
#  Usage:
#  Run the script with an optional argument to set the file permissions for the copied files.
#  If no argument is provided, the default permission setting from the configuration file will be used.
#      ./sd_extract.sh [permissions]
#
#  Configuration file ('sd_extract.conf') requirements:
#  - SOURCE_DIRS: Space-separated list of source directories.
#  - FILE_PATTERNS: Space-separated list of file patterns to sync.
#  - DEST_DIR: Destination directory for synced files.
#  - DEFAULT_PERMISSIONS: Default file permissions if not overridden by command-line argument.
#
#  Notes:
#  - Ensure 'rsync' and 'chmod' commands are available on the system.
#  - Run this script with sufficient permissions to access source directories and write to the destination directory.
#  - The permissions argument must be a 3-digit octal number. Any other format will result in an error.
#
#  Error Conditions:
#  1. No matching files found to copy.
#  2. Destination directory does not exist.
#  5. Configuration file not found.
#  6. Configuration variables not set.
#  7. One or more files failed to copy.
#  8. Invalid permissions argument (not a 3-digit octal number).
#  126. Required command(s) not executable.
#  127. Required command(s) not installed.
#
########################################################################

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

# Check if necessary commands are available
check_commands rsync find chmod

# Determine the script's directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Load configuration from a .conf file
CONF_FILE="$SCRIPT_DIR/etc/sd_extract.conf"
if [ ! -f "$CONF_FILE" ]; then
    CONF_FILE="$SCRIPT_DIR/../etc/sd_extract.conf"
    if [ ! -f "$CONF_FILE" ]; then
        echo "Configuration file not found." >&2
        exit 5
    fi
fi
. "$CONF_FILE"

# Check if necessary variables are set
if [ -z "$SOURCE_DIRS" ] || [ -z "$FILE_PATTERNS" ] || [ -z "$DEST_DIR" ] || [ -z "$DEFAULT_PERMISSIONS" ]; then
    echo "Error: Configuration variables not set. Check sd_extract.conf." >&2
    exit 6
fi

# Check if destination directory exists
if [ ! -d "$DEST_DIR" ]; then
    echo "Error: Destination directory $DEST_DIR does not exist." >&2
    exit 2
fi

# Set default permissions from configuration file or use the first argument if provided
permissions=${1:-$DEFAULT_PERMISSIONS}

# Check if the permissions argument is a valid 3-digit number
if ! echo "$permissions" | grep -Eq '^[0-7]{3}$'; then
    echo "Error: Permissions must be a 3-digit octal number." >&2
    exit 8
fi

# Initialize a flag to check if any files were copied
files_copied=false
# Initialize a variable to track files that failed to sync
error_files=""

# A temporary flag file is used instead of a variable to detect if any files have been copied.
# This approach is necessary because the 'find ... | while read' loop runs in a subshell due to the pipeline.
# Variables set in a subshell are not visible in the parent shell, so changes to variables inside the loop do not persist outside of it.
# By using a flag file, we can create a persistent indicator that can be checked outside of the subshell.
sync_files() {
    local source_dir=$1
    local file_pattern=$2
    local dest_dir=$3
    local permissions=$4
    # Create a temporary flag file to detect if any files have been copied
    local flag_file="/tmp/files_copied_$$"

    # Check if the source directory exists
    if [ ! -d "$source_dir" ]; then
        # Skip the loop if the source directory doesn't exist
        return
    fi

    # Find files matching the pattern in the source directory and sync each file individually
    find "$source_dir" -name "$file_pattern" 2>/dev/null | while IFS= read -r file; do
        echo "Synchronizing $file to $dest_dir..."
        rsync -avz "$file" "$dest_dir/"
        if [ $? -eq 0 ]; then
            # Set permissions for the copied file
            chmod "$permissions" "$dest_dir/$(basename "$file")"
            if [ $? -eq 0 ]; then
                # If the file is successfully copied, create a flag file
                touch "$flag_file"
            else
                echo "Error: Failed to set permissions for $dest_dir/$(basename "$file")"
                error_files="$error_files $file"
            fi
        else
            echo "Error: Rsync failed for $file. Skipping."
            error_files="$error_files $file"
        fi
    done

    # Check for the flag file to determine if files_copied should be set to true.
    if [ -f "$flag_file" ]; then
        files_copied=true
        # Remove the flag file after setting the flag
        rm "$flag_file"
    fi
}

# Loop through each source directory and file pattern defined in the configuration
# Temporarily change IFS (Internal Field Separator) to space to treat the space-separated
# strings in SOURCE_DIRS and FILE_PATTERNS as lists
OLD_IFS="$IFS"
IFS=' '
for source_dir in $SOURCE_DIRS; do
    for file_pattern in $FILE_PATTERNS; do
        # Call sync_files function for each combination of source directory and file pattern
        sync_files "$source_dir" "$file_pattern" "$DEST_DIR" "$permissions"
    done
done
# Restore the original IFS value to avoid affecting subsequent script behavior
IFS="$OLD_IFS"

# Check if any files were copied
if [ "$files_copied" = false ]; then
    echo "No matching files found to copy."
    exit 1
fi

# If there are error files, print them and exit with a non-zero status
if [ -n "$error_files" ]; then
    echo "The following files failed to sync:"
    for error_file in $error_files; do
        echo "$error_file"
    done
    exit 7
else
    echo "Operation completed successfully."
fi

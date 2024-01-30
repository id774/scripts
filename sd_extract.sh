#!/bin/sh

########################################################################
# sd_extract.sh: Extract data from SD cards
#
#  Description:
#  This script synchronizes specified file types from multiple source directories
#  on SD cards to a destination directory on the local machine. It reads source
#  directories, file patterns, and the destination directory from an external
#  configuration file. Files are copied only if they exist in the source directories.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
#  Run the script without any arguments. Ensure that the sync.conf file is properly
#  configured with SOURCE_DIRS, FILE_PATTERNS, and DEST_DIR variables.
#      ./sd_extract.sh
#
########################################################################

# Determine the script's directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Load configuration from a .conf file
CONF_FILE="$SCRIPT_DIR/etc/sd_extract.conf"
if [ ! -f "$CONF_FILE" ]; then
    CONF_FILE="$SCRIPT_DIR/../etc/sd_extract.conf"
    if [ ! -f "$CONF_FILE" ]; then
        echo "Configuration file not found."
        exit 5
    fi
fi
. "$CONF_FILE"

# Check if destination directory exists
if [ ! -d "$DEST_DIR" ]; then
    echo "Error: Destination directory $DEST_DIR does not exist."
    exit 3
fi

# A temporary flag file is used instead of a variable to detect if any files have been copied.
# This approach is necessary because the 'find ... | while read' loop runs in a subshell due to the pipeline.
# Variables set in a subshell are not visible in the parent shell, so changes to variables inside the loop do not persist outside of it.
# By using a flag file, we can create a persistent indicator that can be checked outside of the subshell.
sync_files() {
    local source_dir=$1
    local file_pattern=$2
    local dest_dir=$3
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
            # If the file is successfully copied, create a flag file
            touch "$flag_file"
        else
            echo "Error: Rsync failed for $file."
            exit 2
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
        sync_files "$source_dir" "$file_pattern" "$DEST_DIR"
    done
done
# Restore the original IFS value to avoid affecting subsequent script behavior
IFS="$OLD_IFS"

# Check if any files were copied
if [ "$files_copied" = false ]; then
    echo "No matching files found to copy."
    exit 1
else
    echo "Operation completed successfully."
fi


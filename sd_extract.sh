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

# Initialize a flag to check if any files were copied
files_copied=false

# Function to sync files from source to destination
sync_files() {
    local source_dir=$1
    local file_pattern=$2
    local dest_dir=$3

    # Check if source directory exists
    if [ ! -d "$source_dir" ]; then
        # Skip non-existent source directory
        return
    fi

    # Find files matching the pattern in source directory
    files=$(find "$source_dir" -name "$file_pattern" 2>/dev/null)

    # Check if any files were found
    if [ -z "$files" ]; then
        # Skip if no files match the pattern
        return
    fi

    # Set the flag to true as files are being copied
    files_copied=true

    # Sync each found file
    for file in $files; do
        echo "Synchronizing $file to $dest_dir..."
        rsync -avz "$file" "$dest_dir/"
        if [ $? -ne 0 ]; then
            echo "Error: Rsync failed for $file."
            exit 2
        fi
    done
}

# Loop through source directories and file patterns to sync
for source_dir in "${SOURCE_DIRS[@]}"; do
    for file_pattern in "${FILE_PATTERNS[@]}"; do
        sync_files "$source_dir" "$file_pattern" "$DEST_DIR"
    done
done

# Check if any files were copied
if [ "$files_copied" = false ]; then
    echo "No matching files found to copy."
else
    echo "Operation completed successfully."
fi


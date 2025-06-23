#!/bin/sh

########################################################################
# dashcam_sync.sh: Dashcam Files Management Script
#
#  Description:
#  This script synchronizes dashcam files from a local directory to an
#  external drive and organizes them into a yearly structured folder. It
#  requires a configuration file named 'dashcam_sync.conf' for specifying
#  source and destination directories.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.7 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.6 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.5 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.4 2025-03-17
#       Encapsulated all logic into functions and introduced main function.
#  v1.3 2025-03-16
#       Redirected error messages to stderr for better logging and debugging.
#       Make POSIX compliant by removing 'local' variables.
#  v1.2 2024-02-08
#       Enhanced documentation, added configuration variable checks, and
#       improved error handling and script structure.
#  v1.1 2023-12-23
#       Updated to load source and destination directories from an external
#       configuration file located in 'etc' or '../etc'.
#       Added file existence check in move_files function to prevent errors
#       when no files are available to move.
#  v1.0 2023-12-05
#       Initial release. Adds directory checks, error handling, and
#       improves script reusability.
#
#  Usage:
#  Run the script without any arguments. Ensure that 'dashcam_sync.conf'
#  is properly set up with SOURCE_DIR and DEST_DIR variables.
#      ./dashcam_sync.sh
#
#  Configuration file ('dashcam_sync.conf') requirements:
#  - SOURCE_DIR: Directory containing the dashcam files to be synchronized.
#  - DEST_DIR: Destination directory on the external drive for synchronized files.
#  Ensure both variables are set in 'dashcam_sync.conf'.
#
#  Notes:
#  - Both source and destination directories must exist and be writable.
#  - Files are first synced to a 'daily' subdirectory, then moved to a yearly directory.
#
#  Error Conditions:
#  1. Source or destination directory does not exist.
#  2. Rsync operation failed.
#  3. Moving files failed.
#  4. Configuration file not found.
#  5. One or more configuration variables not set.
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

# Determine the script's directory
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

# Function to load configuration
load_configuration() {
    CONF_FILE="$SCRIPT_DIR/etc/dashcam_sync.conf"
    if [ ! -f "$CONF_FILE" ]; then
        CONF_FILE="$SCRIPT_DIR/../etc/dashcam_sync.conf"
        if [ ! -f "$CONF_FILE" ]; then
            echo "[ERROR] Configuration file not found." >&2
            exit 4
        fi
    fi
    . "$CONF_FILE"

    # Check if necessary variables are set
    if [ -z "$SOURCE_DIR" ] || [ -z "$DEST_DIR" ]; then
        echo "[ERROR] SOURCE_DIR or DEST_DIR not set in configuration." >&2
        exit 5
    fi
}

# Function to check if source and destination directories exist
check_directories() {
    if [ ! -d "$SOURCE_DIR" ] || [ ! -d "$DEST_DIR" ]; then
        echo "[ERROR] Source or destination directory does not exist." >&2
        exit 1
    fi
}

# Function to synchronize files using rsync
sync_files() {
    echo "[INFO] Synchronizing files to $DEST_DIR..."
    rsync -avz --delete "$SOURCE_DIR/" "$DEST_DIR/daily/"
    if [ $? -ne 0 ]; then
        echo "[ERROR] Rsync failed." >&2
        exit 2
    fi
}

# Function to move files to a yearly directory
move_files() {
    src="$1"
    dest="$2"

    # Check if there are files to move
    if [ -z "$(find "$src" -type f | head -n 1)" ]; then
        echo "[WARN] No files to move from $src." >&2
        return 0
    fi

    echo "Moving files from '$src' to '$dest'..."
    mv "$src"/* "$dest/" 2>/dev/null
    if [ $? -ne 0 ]; then
        echo "[ERROR] Moving files failed." >&2
        exit 3
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    load_configuration
    check_directories

    YEAR_DIR="$(date +"%Y")"

    sync_files
    move_files "$DEST_DIR/daily" "$DEST_DIR/$YEAR_DIR"
    move_files "$SOURCE_DIR" "$SOURCE_DIR/../$YEAR_DIR"

    echo "[INFO] Operation completed successfully."
}

# Execute main function
main "$@"

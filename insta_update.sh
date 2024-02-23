#!/bin/sh

########################################################################
# insta_update.sh: Instagram Content Update Script for Subdirectories
#
#  Description:
#  This script navigates to a specified directory and executes an Instagram
#  content downloader script in each subdirectory, followed by a synchronization
#  script. It supports an optional reset feature to clear each target directory
#  before downloading new content. Instead of directly clearing the content,
#  it now safely renames the target directory and only removes it after a
#  successful update, ensuring data integrity even if the download fails.
#  The script requires a configuration file named 'insta_update.conf' to
#  specify necessary settings. It is designed to be run in a POSIX-compliant
#  shell environment.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2024-02-23
#       Added check_commands function to verify the presence and executability
#       of required system commands before proceeding with the main script.
#  v1.1 2024-02-21
#       Updated the reset functionality to rename the target directory before
#       clearing, ensuring data is not lost if the download fails.
#  v1.0 2024-02-19
#       Initial release.
#
#  Usage:
#  ./insta_update.sh [--reset]
#  The --reset option is powerful and will now safely rename the target
#  directory before downloading new content. The original directory will only be
#  removed if the download succeeds, ensuring data integrity.
#
#  Configuration file ('insta_update.conf') requirements:
#  - PYTHON_BIN: Path to the Python binary.
#  - DOWNLOADER_SCRIPT: Path to the Instagram downloader script.
#  - SYNC_SCRIPT: Path to the Instagram synchronization script.
#  - TARGET_DIR: Directory containing Instagram subdirectories.
#  Ensure all these variables are set in 'insta_update.conf'.
#
#  Notes:
#  - The --reset option now renames the target directory to a temporary one
#    before downloading. If the download fails, the original content is restored.
#  - Make sure 'exclude_accounts.txt' and 'include_accounts.txt' are properly
#    formatted, with one account name per line, if they are used.
#  - The script supports 'exclude_accounts.txt' and 'include_accounts.txt' lists.
#    If 'exclude_accounts.txt' is present, any subdirectories matching the names
#    in this list will be skipped. If 'include_accounts.txt' is present, only the
#    subdirectories listed will be processed. If a subdirectory is listed in both,
#    it will be excluded.
#
#  Error Conditions:
#  1. Configuration file not found or incomplete: The script will terminate if
#     it cannot find 'insta_update.conf' or if any required variables are unset.
#  2. Unknown command-line options: If any unrecognized options are provided,
#     the script will display an error message and exit.
#  3. Necessary configuration variables not set: The script checks if all
#     necessary configuration variables are set in 'insta_update.conf'.
#  4. Specified scripts or target directory do not exist or are not executable.
#
########################################################################

# Determine the script's directory
SCRIPT_DIR=$(dirname "$0")

# Load configuration from a .conf file
CONF_FILE="$SCRIPT_DIR/etc/insta_update.conf"
if [ ! -f "$CONF_FILE" ]; then
    CONF_FILE="$SCRIPT_DIR/../etc/insta_update.conf"
    if [ ! -f "$CONF_FILE" ]; then
        echo "Error: Configuration file not found."
        exit 1
    fi
fi
. "$CONF_FILE"

# Check if necessary variables are set
if [ -z "$PYTHON_BIN" ] || [ -z "$DOWNLOADER_SCRIPT" ] || [ -z "$SYNC_SCRIPT" ] || [ -z "$TARGET_DIR" ]; then
    echo "Error: One or more configuration variables are not set. Please check your insta_update.conf."
    exit 3
fi

# Check if specified scripts and target directory exist and are executable
if [ ! -x "$PYTHON_BIN" ] || [ ! -f "$DOWNLOADER_SCRIPT" ] || [ ! -x "$DOWNLOADER_SCRIPT" ] || [ ! -f "$SYNC_SCRIPT" ] || [ ! -x "$SYNC_SCRIPT" ] || [ ! -d "$TARGET_DIR" ]; then
    echo "Error: Specified scripts or target directory do not exist or are not executable."
    exit 4
fi

# Check for required commands
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again."
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions."
            exit 126
        fi
    done
}

# Ensure necessary commands are available
check_commands mv mkdir rm cd grep

RESET=false

# Parse options
while [ $# -gt 0 ]; do
    case $1 in
        --reset)
        RESET=true
        shift
        ;;
        *)
        # Unknown option
        echo "Error: Unknown option $1"
        exit 2
        ;;
    esac
done

update_content() {
    subdir="$1"

    if [ "$RESET" = true ]; then
        echo "Resetting directory: $subdir"
        subdir_basename=$(basename "$subdir")  # Extract the directory name
        old_dir="${TARGET_DIR}/${subdir_basename}_old"

        if [ -d "$old_dir" ]; then
            echo "Removing existing backup directory: ${subdir_basename}_old"
            rm -rf "$old_dir"  # Remove existing backup directory if it exists
        fi

        mv "$subdir" "$old_dir"  # Rename the directory to *_old
        mkdir -p "$subdir"  # Create a new directory with the original name
    fi

    echo "Running: $PYTHON_BIN $DOWNLOADER_SCRIPT in $subdir"
    cd "$subdir" || exit
    "$PYTHON_BIN" "$DOWNLOADER_SCRIPT" || exit
    cd "$TARGET_DIR" || exit
    echo "Synchronizing: $SYNC_SCRIPT $(basename "$subdir")"
    "$SYNC_SCRIPT" "$(basename "$subdir")" || exit
}

should_process() {
    EXCLUDE_LIST="$TARGET_DIR/exclude_accounts.txt"
    INCLUDE_LIST="$TARGET_DIR/include_accounts.txt"

    local subdir_name=$(basename "$1")
    # Skip if subdir is in exclude list
    if [ -f "$EXCLUDE_LIST" ] && grep -qx "$subdir_name" "$EXCLUDE_LIST"; then
        return 1
    fi
    # Process if subdir is in include list
    if [ -f "$INCLUDE_LIST" ]; then
        if grep -qx "$subdir_name" "$INCLUDE_LIST"; then
            return 0
        else
            return 1  # Skip if subdir is not in include list
        fi
    fi
    return 0  # Default to process if no include list is provided
}

cd "$TARGET_DIR" || exit
for subdir in */ ; do
    if [ -d "$subdir" ] && should_process "$subdir"; then
        update_content "$subdir"
    fi
done


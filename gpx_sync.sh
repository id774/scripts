#!/bin/sh

########################################################################
# gpx_sync.sh: GPX File Management Script
#
#  Description:
#  This script manages GPX files by performing operations like copying to
#  specific directories, syncing with a remote server, and cleaning up
#  temporary files. It requires a configuration file named 'gpx_sync.conf' to
#  specify necessary settings such as directories and remote server information.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.5 2024-02-08
#       Updated to load configuration from an external file. Added checks for
#       necessary configuration variables. Improved error handling.
#  v1.4 2023-12-23
#       Updated to load configuration from an external file.
#       Refactored for POSIX compliance. Replaced Bash-specific syntax
#       with POSIX standard commands and structures. Enhanced portability
#       and compatibility across different UNIX-like systems.
#  v1.3 2023-12-19
#       Modified copy_files function to return an error if the destination
#       directory does not exist.
#  v1.2 2023-12-05
#       Enhanced error handling and return codes for each function.
#  v1.1 2023-12-01
#       Improved handling of multiple GPX files with enhanced file copying
#       and deletion functionality.
#  v1.0 2023-11-24
#       Initial release.
#
#  Usage:
#  Run the script without any arguments. Ensure that the 'gpx_sync.conf' file
#  is properly configured with the necessary variables.
#      ./gpx_sync.sh
#
#  Configuration file ('gpx_sync.conf') requirements:
#  - TMP_DIR: Temporary directory for GPX files.
#  - USER_GPX_DIR: User-specific GPX directory.
#  - MOUNTED_DIR: Mounted directory for backups.
#  - RSYNC_USER: Username for remote server access.
#  - RSYNC_HOST: Hostname or IP address of the remote server.
#  Ensure all these variables are set in 'gpx_sync.conf'.
#
#  Notes:
#  - Ensure that all specified directories exist and are writable.
#  - The script updates file permissions as needed and performs clean-up operations.
#  - Remote synchronization is attempted only if the remote server is reachable.
#
#  Error Conditions:
#  1. No GPX files found in the specified temporary directory.
#  2. Destination directory for copying files does not exist.
#  3. Configuration file not found.
#  4. Necessary configuration variable(s) not set.
#
########################################################################

# Determine the script's directory
SCRIPT_DIR=$(dirname "$0")

# Load configuration from a .conf file
CONF_FILE="$SCRIPT_DIR/etc/gpx_sync.conf"
if [ ! -f "$CONF_FILE" ]; then
    CONF_FILE="$SCRIPT_DIR/../etc/gpx_sync.conf"
    if [ ! -f "$CONF_FILE" ]; then
        echo "Error: Configuration file not found."
        exit 3
    fi
fi
. "$CONF_FILE"

# Check if necessary variables are set
if [ -z "$TMP_DIR" ] || [ -z "$USER_GPX_DIR" ] || [ -z "$MOUNTED_DIR" ] || [ -z "$RSYNC_USER" ] || [ -z "$RSYNC_HOST" ]; then
    echo "Error: One or more configuration variables are not set. Please check your gpx_sync.conf."
    exit 4
fi

CURRENT_YEAR=$(date +"%Y")

# Function to check if GPX files exist in a directory
check_gpx_files() {
    local dir=$1
    if [ -z "$(ls -A $dir/*.gpx 2>/dev/null)" ]; then
        echo "No GPX files found in $dir. Exiting."
        return 1
    fi
    return 0
}

# Function to copy files to a directory, returning an error if the directory does not exist
copy_files() {
    local source_dir=$1
    local destination=$2
    if [ ! -d "$destination" ]; then
        echo "Error: Destination directory $destination does not exist."
        return 2
    fi
    echo "Copying files from $source_dir to $destination"
    cp "$source_dir"/*.gpx "$destination" || return $?
}

# Function to perform rsync
sync_files() {
    local source=$1
    local destination_user=$2
    local destination_host=$3
    echo "rsync -avz --delete $source $destination_user@$destination_host:~/gpx/"
    rsync -avz --delete "$source" "$destination_user@$destination_host:~/gpx/" || return $?
}

# Function to remove files
remove_files() {
    for file in "$@"; do
        echo "Removing file: $file"
        rm -v "$file" || return $?
    done
}

# Main logic
check_gpx_files "$TMP_DIR" || exit $?

copy_files "$TMP_DIR" "$HOME/$USER_GPX_DIR/$CURRENT_YEAR/" || exit $?

copy_files "$TMP_DIR" "$MOUNTED_DIR/$USER_GPX_DIR/$CURRENT_YEAR/" || exit $?

sync_files "$HOME/$USER_GPX_DIR" "$RSYNC_USER" "$RSYNC_HOST" || exit $?

remove_files "$TMP_DIR"/*.gpx || exit $?

echo "All operations completed successfully."


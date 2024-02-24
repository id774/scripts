#!/bin/sh

########################################################################
# gpx_sync.sh: GPX File Management Script
#
#  Description:
#  This script manages GPX files by performing operations like copying to
#  specific directories, syncing with a remote server, cleaning up
#  temporary files, and setting file permissions. It requires a configuration
#  file named 'gpx_sync.conf' to specify necessary settings such as directories,
#  remote server information, and default file permissions. The script allows
#  overriding the default file permissions via a command-line argument.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.7 2024-02-24
#       Added check_commands function to ensure all required system commands
#       are installed and executable before proceeding with file operations.
#  v1.6 2024-02-14
#       Added functionality to set file permissions for GPX files before
#       copying them to the destination directories. Permissions can be
#       specified via a command-line argument or through the configuration
#       file. Improved error handling for missing configuration settings.
#  v1.5 2024-02-08
#       Added checks for necessary configuration variables and improved error handling.
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
#  Run the script with an optional argument to set the file permissions for the copied files.
#  If no argument is provided, the default permission setting from the configuration file will be used.
#      ./gpx_sync.sh [permissions]
#
#  Configuration file ('gpx_sync.conf') requirements:
#  - TMP_DIR: Temporary directory for GPX files.
#  - USER_GPX_DIR: User-specific GPX directory.
#  - MOUNTED_DIR: Mounted directory for backups.
#  - RSYNC_USER: Username for remote server access.
#  - RSYNC_HOST: Hostname or IP address of the remote server.
#  - DEFAULT_PERMISSIONS: Default file permissions if not overridden by command-line argument.
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
#  5. DEFAULT_PERMISSIONS not set in configuration file when no permissions argument provided.
#  126. Required command(s) not executable.
#  127. Required command(s) not installed.
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

# Check if DEFAULT_PERMISSIONS is set in the configuration file if no permissions are provided as an argument
if [ -z "$1" ] && [ -z "$DEFAULT_PERMISSIONS" ]; then
    echo "Error: DEFAULT_PERMISSIONS is not set in the configuration file and no permissions argument was provided."
    exit 5
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
check_commands chmod cp rsync rm

# Set default permissions from the configuration file or use the first argument if provided
permissions=${1:-$DEFAULT_PERMISSIONS}

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

# Function to set permissions and copy files to a directory, returning an error if the directory does not exist
copy_files() {
    local source_dir=$1
    local destination=$2
    local permissions=$3
    if [ ! -d "$destination" ]; then
        echo "Error: Destination directory $destination does not exist."
        return 2
    fi
    echo "Copying files from $source_dir to $destination"
    for file in "$source_dir"/*.gpx; do
        chmod "$permissions" "$file"
        cp "$file" "$destination" || return $?
    done
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

copy_files "$TMP_DIR" "$HOME/$USER_GPX_DIR/$CURRENT_YEAR/" "$permissions" || exit $?

copy_files "$TMP_DIR" "$MOUNTED_DIR/$USER_GPX_DIR/$CURRENT_YEAR/" "$permissions" || exit $?

sync_files "$HOME/$USER_GPX_DIR" "$RSYNC_USER" "$RSYNC_HOST" || exit $?

remove_files "$TMP_DIR"/*.gpx || exit $?

echo "All operations completed successfully."


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
#  Multi-host sync:
#  - Set RSYNC_HOSTS as a space-separated list (e.g., "harpuia wyvern") to rsync
#    to multiple hosts. If RSYNC_HOSTS is unset, the legacy RSYNC_HOST is used.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
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
#  - RSYNC_HOSTS: Space-separated hostnames or IPs for remote sync (preferred).
#                 Example: RSYNC_HOSTS="server1 server2"
#    (Fallback) RSYNC_HOST: Single hostname or IP if RSYNC_HOSTS is not set.
#  - DEFAULT_PERMISSIONS: Default file permissions if not overridden by command-line argument.
#  Ensure all these variables are set in 'gpx_sync.conf'.
#
#  Notes:
#  - Ensure that all specified directories exist and are writable.
#  - Remote synchronization runs for each host in RSYNC_HOSTS, or for RSYNC_HOST if provided.
#  - The script updates file permissions as needed and performs clean-up operations.
#  - Remote synchronization is attempted only if the remote server is reachable.
#  - The permissions argument must be a 3-digit octal number. Any other format will result in an error.
#
#  Error Conditions:
#  1. No GPX files found in the specified temporary directory.
#  2. Destination directory for copying files does not exist.
#  3. Configuration file not found.
#  4. Necessary configuration variable(s) not set.
#  5. DEFAULT_PERMISSIONS not set in configuration file when no permissions argument provided.
#  6. Invalid permissions argument (not a 3-digit octal number).
#  126. Required command(s) not executable.
#  127. Required command(s) not installed.
#
#  Version History:
#  v2.4 2025-09-22
#       Add multi host rsync via RSYNC_HOSTS with fallback to RSYNC_HOST and update header docs accordingly.
#  v2.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v2.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v2.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v2.0 2025-03-17
#       Encapsulated all logic into functions and introduced main function.
#  v1.9 2025-03-14
#       Redirected error messages to stderr for better logging and debugging.
#       Refactored for POSIX compliance. Replaced Bash-specific syntax
#       with POSIX standard commands and structures. Enhanced portability
#       and compatibility across different UNIX-like systems.
#  v1.8 2024-08-23
#       Added validation to ensure that the permissions argument is a 3-digit octal number.
#       Updated error handling with a new return code (6) for invalid permissions input.
#       Added a note to restrict permissions argument to a 3-digit octal number.
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

# Check for required commands
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

# Check if GPX files exist in a directory
check_gpx_files() {
    dir="$1"
    if ! find "$dir" -maxdepth 1 -name '*.gpx' -type f | grep -q .; then
        echo "[ERROR] No GPX files found in $dir. Exiting." >&2
        return 1
    fi
    return 0
}

# Parse arguments
parse_arguments() {
    # Set default permissions from config or use the argument
    permissions=${1:-$DEFAULT_PERMISSIONS}

    # Check if DEFAULT_PERMISSIONS is set in the configuration file if no permissions are provided
    if [ -z "$permissions" ]; then
        echo "[ERROR] DEFAULT_PERMISSIONS is not set in the configuration file and no permissions argument was provided." >&2
        exit 5
    fi

    # Validate permissions format (3-digit octal number)
    if echo "$permissions" | grep -E '^[0-7][0-7][0-7]$' >/dev/null 2>&1; then
        :
    else
        echo "[ERROR] Permissions must be a 3-digit octal number." >&2
        exit 6
    fi
}

# Load configuration
load_configuration() {
    CONF_FILE="$SCRIPT_DIR/etc/gpx_sync.conf"
    if [ ! -f "$CONF_FILE" ]; then
        CONF_FILE="$SCRIPT_DIR/../etc/gpx_sync.conf"
        if [ ! -f "$CONF_FILE" ]; then
            echo "[ERROR] Configuration file not found." >&2
            exit 3
        fi
    fi
    . "$CONF_FILE"


    # Check basic variables
    if [ -z "$TMP_DIR" ] || [ -z "$USER_GPX_DIR" ] || [ -z "$MOUNTED_DIR" ] || [ -z "$RSYNC_USER" ]; then
        echo "[ERROR] One or more configuration variables are not set. Please check your gpx_sync.conf." >&2
        exit 4
    fi

    # Build host list: prefer RSYNC_HOSTS, fallback to RSYNC_HOST
    HOST_LIST=""
    if [ -n "$RSYNC_HOSTS" ]; then
        HOST_LIST="$RSYNC_HOSTS"
    elif [ -n "$RSYNC_HOST" ]; then
        HOST_LIST="$RSYNC_HOST"
    else
        echo "[ERROR] Neither RSYNC_HOSTS nor RSYNC_HOST is set in the configuration." >&2
        exit 4
    fi

}

# Set permissions and copy files to a directory
copy_files() {
    source_dir="$1"
    destination="$2"
    permissions="$3"
    if [ ! -d "$destination" ]; then
        echo "[ERROR] Destination directory $destination does not exist." >&2
        return 2
    fi
    echo "[INFO] Copying files from $source_dir to $destination"
    find "$source_dir" -maxdepth 1 -name '*.gpx' -type f -exec chmod "$permissions" {} \; -exec cp {} "$destination" \;
}

# Sync to all configured hosts
sync_all_hosts() {
    source="$1"
    destination_user="$2"
    rc=0
    for h in $HOST_LIST; do
        sync_files "$source" "$destination_user" "$h" || rc=$?
        [ "$rc" -ne 0 ] && break
    done
    return "$rc"
}

# Perform rsync
sync_files() {
    source="$1"
    destination_user="$2"
    destination_host="$3"
    echo "[INFO] rsync -avz --delete $source $destination_user@$destination_host:~/gpx/"
    rsync -avz --delete "$source" "$destination_user@$destination_host:~/gpx/" || return $?
}

# Remove files
remove_files() {
    for file in "$@"; do
        echo "[INFO] Removing file: $file"
        rm -v "$file" || return $?
    done
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Determine the script's directory
    SCRIPT_DIR=$(dirname "$0")

    load_configuration
    check_commands chmod cp rsync rm find
    parse_arguments "$@"

    CURRENT_YEAR=$(date +"%Y")

    check_gpx_files "$TMP_DIR" || exit $?
    copy_files "$TMP_DIR" "$HOME/$USER_GPX_DIR/$CURRENT_YEAR/" "$permissions" || exit $?
    copy_files "$TMP_DIR" "$MOUNTED_DIR/$USER_GPX_DIR/$CURRENT_YEAR/" "$permissions" || exit $?
    sync_all_hosts "$HOME/$USER_GPX_DIR" "$RSYNC_USER" || exit $?
    remove_files "$TMP_DIR"/*.gpx || exit $?

    echo "[INFO] All operations completed successfully."
    return 0
}

# Execute main function
main "$@"

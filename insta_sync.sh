#!/bin/sh

########################################################################
# insta_sync.sh: Instagram Account Data Sync Script
#
#  Description:
#  This script is designed to manage and synchronize Instagram account data.
#  It updates directory permissions, syncs data with a local backup directory,
#  and conditionally syncs with a remote server if reachable. The script operates
#  on a per-account basis, with each account's data stored in its own directory.
#  It requires a configuration file named 'insta_sync.conf' for specifying
#  various settings. The script checks if all necessary configuration variables
#  are set and terminates with an error if any are missing.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script with the Instagram account name as an argument. Make sure
#  the configuration file 'insta_sync.conf' is properly set up in the same
#  directory as this script.
#      ./insta_sync.sh <instagram_account_name>
#
#  Options:
#    --help, -h       Display this help and exit.
#
#  Configuration file ('insta_sync.conf') requirements:
#  - INSTA_DIR: Base directory for Instagram account data.
#  - BACKUP_DIR: Local backup directory.
#  - REMOTE_USER: Username for remote server access.
#  - REMOTE_HOST: Hostname or IP address of the remote server.
#  - REMOTE_DIR: Remote directory for data synchronization.
#  - DIR_PERMISSIONS: Permissions for directories within INSTA_DIR.
#  - FILE_PERMISSIONS: Permissions for files within INSTA_DIR.
#  Ensure all these variables are set in 'insta_sync.conf'.
#
#  Notes:
#  - Ensure the specified Instagram account directory exists within INSTA_DIR.
#  - The script updates file permissions recursively within the account directory.
#  - Local backup directory must exist prior to running this script.
#  - Remote sync is attempted only if the remote server is reachable.
#
#  Error Conditions:
#  1. No argument provided.
#  2. Instagram account directory does not exist.
#  3. Local backup directory does not exist.
#  4. Configuration file not found.
#  5. One or more configuration variables not set.
#  126. Required command(s) not executable.
#  127. Required command(s) not installed.
#
#  Version History:
#  v2.0 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.9 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.8 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.7 2025-03-17
#       Encapsulated all logic into functions and introduced main function.
#  v1.6 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.5 2024-10-23
#       Fixed issue with removing trailing backslash from the Instagram account name argument.
#       Added 'sed' to the list of required commands in the check_commands function
#       to ensure the script checks for its availability before execution.
#  v1.4 2024-06-18
#       Added --help and -h options to display help message.
#  v1.3 2024-05-05
#       Modified to remove trailing backslash from the Instagram account name argument.
#  v1.2 2024-03-07
#       Added configuration options for customizable file and directory
#       permissions in 'insta_sync.conf'. Updated script to apply these
#       permissions to Instagram account data directories and files.
#  v1.1 2024-02-23
#       Added check_commands function to verify the presence and executability
#       of required system commands before proceeding with the main script.
#  v1.0 2024-02-08
#       Initial release. Added support for per-account directory handling,
#       local and conditional remote synchronization. Added checks for necessary
#       configuration variables.
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

# Load configuration
load_config() {
    # Determine the script's directory
    SCRIPT_DIR=$(dirname "$0")

    # Load configuration from a .conf file
    CONF_FILE="$SCRIPT_DIR/etc/insta_sync.conf"
    [ ! -f "$CONF_FILE" ] && CONF_FILE="$SCRIPT_DIR/../etc/insta_sync.conf"
    if [ ! -f "$CONF_FILE" ]; then
        echo "[ERROR] Configuration file not found." >&2
        exit 4
    fi
    . "$CONF_FILE"
}

# Validate configuration
validate_config() {
    # Check if necessary variables are set
    if [ -z "$INSTA_DIR" ] || [ -z "$BACKUP_DIR" ] || \
       [ -z "$REMOTE_USER" ] || [ -z "$REMOTE_HOST" ] || [ -z "$REMOTE_DIR" ]; then
        echo "[ERROR] Configuration variables not set." >&2
        exit 5
    fi
}

# Check if a directory exists
check_dir() {
    # Checks if the specified directory exists
    if [ ! -d "$1" ]; then
        echo "[ERROR] Directory $1 does not exist." >&2
        exit 2
    fi
}

# Synchronize files
sync_files() {
    # Syncs files from source to destination directory
    echo "[INFO] Syncing files from $1 to $2..."
    rsync -avz --delete "$1" "$2" || exit 3
}

# Check and perform remote synchronization
check_remote_sync() {
    # Checks remote server reachability and performs sync if possible
    if ping -c 1 "$REMOTE_HOST" >/dev/null 2>&1; then
        echo "[INFO] Remote server reachable. Syncing data..."
        sync_files "$1" "$REMOTE_USER@$REMOTE_HOST:$REMOTE_DIR/"
    else
        echo "[WARN] Remote server not reachable. Skipping remote sync." >&2
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Check for Instagram account name argument
    if [ -z "$1" ]; then
        echo "[ERROR] No Instagram account name provided." >&2
        exit 1
    fi

    # Load configuration
    load_config
    validate_config

    # Ensure necessary commands are available
    check_commands rsync ping chmod find sed

    # Retrieve the Instagram account name argument, remove trailing backslash if exists
    ACCOUNT_NAME=$(echo "$1" | sed 's/\\$//')
    INSTA_ACCOUNT_DIR="$INSTA_DIR/$ACCOUNT_NAME"
    BACKUP_ACCOUNT_DIR="$BACKUP_DIR/"

    # Check required directories
    check_dir "$INSTA_ACCOUNT_DIR"
    check_dir "$BACKUP_DIR"

    # Update permissions
    chmod "$DIR_PERMISSIONS" "$INSTA_ACCOUNT_DIR"
    find "$INSTA_ACCOUNT_DIR" -type f -exec chmod "$FILE_PERMISSIONS" {} \;

    # Perform local sync
    sync_files "$INSTA_ACCOUNT_DIR" "$BACKUP_ACCOUNT_DIR"

    # Attempt remote sync
    check_remote_sync "$INSTA_ACCOUNT_DIR"

    echo "[INFO] Instagram account data sync for '$ACCOUNT_NAME' completed."
    return 0
}

# Execute main function
main "$@"
exit $?

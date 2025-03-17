#!/bin/sh

########################################################################
# get-fastladder-db.sh: Retrieve Fastladder Database from Remote Server
#
#  Description:
#  This script retrieves the Fastladder database from a remote server
#  using `rsync`. It allows specifying a user and host as arguments.
#  - Ensures required commands are available before execution.
#  - Creates a backup of the existing database before overwriting.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-16
#       Added system validation, command checks, and backup handling.
#  v0.1 2016-04-09
#       Initial version.
#
#  Usage:
#  Retrieve the Fastladder database with default user and host:
#      ./get-fastladder-db.sh
#
#  Specify a user and host:
#      ./get-fastladder-db.sh <user> <host>
#
#  Requirements:
#  - Must be executed on Linux or macOS.
#  - Requires `rsync` installed.
#  - SSH access to the specified host is required.
#
########################################################################

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to process arguments
check_arguments() {
    USER="${1:-debian}"
    HOST="${2:-harpuia}"
}

# Function to sync Fastladder database
sync_database() {
    SOURCE_DB_PATH="fastladder/db/new.db"
    TARGET_DB_PATH="$HOME/fastladder/db/fastladder.db"

    # Ensure target directory exists
    mkdir -p "$(dirname "$TARGET_DB_PATH")"

    # Backup existing database if it exists
    if [ -f "$TARGET_DB_PATH" ]; then
        BACKUP_DB_PATH="${TARGET_DB_PATH}.bak.$(date +%Y%m%d%H%M%S)"
        echo "Backing up existing database to $BACKUP_DB_PATH"
        mv "$TARGET_DB_PATH" "$BACKUP_DB_PATH"
    fi

    echo "Syncing Fastladder database from $USER@$HOST..."
    rsync -auvz "$USER@$HOST:~/$SOURCE_DB_PATH" "$TARGET_DB_PATH"

    if [ $? -eq 0 ]; then
        echo "Fastladder database successfully retrieved."
    else
        echo "Error: Failed to retrieve the database from $USER@$HOST" >&2
        exit 1
    fi
}

# Main function to execute the script
main() {
    check_commands rsync mkdir mv
    check_arguments "$@"
    sync_database
}

# Execute main function
main "$@"

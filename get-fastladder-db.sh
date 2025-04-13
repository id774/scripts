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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
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

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

# Function to check required commands
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
        echo "[INFO] Backing up existing database to $BACKUP_DB_PATH"
        mv "$TARGET_DB_PATH" "$BACKUP_DB_PATH"
    fi

    echo "[INFO] Syncing Fastladder database from $USER@$HOST..."
    rsync -auvz "$USER@$HOST:~/$SOURCE_DB_PATH" "$TARGET_DB_PATH"

    if [ $? -eq 0 ]; then
        echo "[INFO] Fastladder database successfully retrieved."
    else
        echo "[ERROR] Failed to retrieve the database from $USER@$HOST" >&2
        exit 1
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_commands rsync mkdir mv
    check_arguments "$@"
    sync_database
}

# Execute main function
main "$@"

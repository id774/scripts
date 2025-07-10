#!/bin/sh

########################################################################
# munin-symlink.sh: Monitor multiple servers and update symlinks
#
#  Description:
#  This script monitors the status of multiple servers by checking the
#  presence and modification time of specific "is_alive" files.
#  If the "is_alive" file has been modified within the last 10 minutes,
#  a symbolic link will be created or updated. If the file has not been
#  modified within the last 10 minutes, the symbolic link will be removed.
#
#  The purpose of this script is to dynamically update the monitoring
#  configuration for Munin, allowing it to accurately reflect the
#  availability of various servers in real-time.
#
#  This script is intended to be executed periodically via cron,
#  typically every 5 minutes.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./munin-symlink.sh
#      This script is intended to be executed periodically by cron.
#
#  Cron Usage:
#      Add the following line to /etc/cron.d/munin-symlink to execute every 5 minutes:
#      */5 * * * * root /path/to/munin-symlink.sh
#
#  Requirements:
#      - Linux system
#      - Required commands: stat, ln, rm, date, find
#
#  Notes:
#      - Ensure that all directories and files are accessible by the user
#        executing this script (typically root or munin user).
#      - Make sure that Munin is configured to include configurations from
#        /etc/munin/munin-conf.d.
#      - Symbolic links in /etc/munin/munin-conf.d point to files in
#        /etc/munin/server-available.
#      - This script only removes symbolic links, it does not delete
#        configuration files from /etc/munin/server-available.
#
#  Version History:
#  v1.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.4 2025-05-16
#       Add return 0 to main and exit $? at script end for consistent exit status.
#  v1.3 2025-05-10
#       Add cron execution check and usage support with unified structure.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-04-10
#       Externalized configuration to munin-symlink.conf and added dynamic server list.
#  v1.0 2025-04-08
#       Initial version implementing symlink control for multiple servers.
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

# Function to check if the script is running from cron
is_running_from_cron() {
    if tty -s; then
        return 1  # Terminal attached (interactive session)
    else
        return 0  # No terminal (likely cron)
    fi
}

# Load configuration from external file
load_config() {
    SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
    CONFIG_FILE="$SCRIPT_DIR/etc/munin-symlink.conf"

    if [ ! -f "$CONFIG_FILE" ]; then
        CONFIG_FILE="$SCRIPT_DIR/../etc/munin-symlink.conf"
    fi

    if [ -f "$CONFIG_FILE" ]; then
        . "$CONFIG_FILE"
    else
        echo "[ERROR] Configuration file not found: $CONFIG_FILE" >&2
        exit 1
    fi
}

# Function to update symlink based on the target file's modification time
update_symlink() {
    # Validate argument count
    if [ "$#" -ne 1 ]; then
        echo "[ERROR] Invalid number of arguments." >&2
        echo "Usage: update_symlink <SERVER_NAME>" >&2
        return 1
    fi

    # Define paths based on the server name
    SERVER_NAME="$1"
    SYMLINK="$MUNIN_CONF_DIR/$SERVER_NAME.conf"
    LINK_TARGET="$ACTIVE_DIR/$SERVER_NAME.conf"
    TARGET_FILE="$RECEIVED_DIR/$SERVER_NAME/${SERVER_NAME}_is_alive"

    # Check if the target file exists
    if [ ! -f "$TARGET_FILE" ]; then
        echo "[ERROR] Target file not found: $TARGET_FILE" >&2
        return 1
    fi

    # Retrieve the current time and the modification time of the target file
    CURRENT_TIME=$(date +%s)
    FILE_TIME=$(stat -c %Y "$TARGET_FILE" 2>/dev/null || stat -f %m "$TARGET_FILE" 2>/dev/null)

    if [ -z "$FILE_TIME" ]; then
        echo "[ERROR] Failed to retrieve modification time for: $TARGET_FILE" >&2
        return 1
    fi

    # Calculate time difference
    TIME_DIFF=$((CURRENT_TIME - FILE_TIME))

    # Update or remove symbolic link based on the file modification time
    if [ "$TIME_DIFF" -le "$TIME_THRESHOLD" ]; then
        if [ ! -L "$SYMLINK" ] || [ "$(readlink "$SYMLINK")" != "$LINK_TARGET" ]; then
            ln -snf "$LINK_TARGET" "$SYMLINK"
            #echo "[INFO] Symlink created or updated: $SYMLINK -> $LINK_TARGET"

            # Remove datafile only if a new symlink was created or modified
            rm -f /var/lib/munin/datafile
        fi
    else
        if [ -L "$SYMLINK" ]; then
            rm "$SYMLINK"
            #echo "[INFO] Symlink removed: $SYMLINK"
        fi
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    if ! is_running_from_cron; then
        echo "[ERROR] This script is intended to be run by cron only." >&2
        exit 1
    fi

    load_config

    for server in $SERVER_LIST; do
        update_symlink "$server"
        #echo "[INFO] Symlink monitoring completed."
    done

    return 0
}

# Execute main function
main "$@"
exit $?

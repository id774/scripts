#!/bin/sh

########################################################################
# munin-sync.sh: Sync Munin data and logs from source server to target server
#
#  Description:
#  This script synchronizes Munin-generated data and various system logs from a source server
#  to a designated target server. It uses rsync for efficient data transfer and includes
#  heartbeat file generation for health check monitoring.
#
#  The script ensures all relevant files are transferred to the remote server's specified
#  directories. Temporary log directories are created if not present, and existing logs are
#  overwritten by each run to maintain up-to-date monitoring.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-04-14
#       Skip sync_munin_data and sync_logs_to_remote if script is running on the target server.
#  v1.1 2025-04-11
#       Added usage() function and --help option. Added Munin directory existence check.
#  v1.0 2025-04-07
#       Initial release. Implements rsync-based data transfer and logging sync with heartbeat file generation.
#
#  Usage:
#      ./munin-sync.sh
#      This script is intended to be executed automatically via cron.
#
#  Cron Usage:
#  Add the following line to /etc/cron.d/munin-sync to execute every 5 minutes:
#      1-56/5 * * * * munin test -x /var/lib/munin/bin/munin-sync.sh && /var/lib/munin/bin/munin-sync.sh
#
#  This ensures the script runs every 5 minutes starting at the 2nd minute of each 5-minute interval,
#  reducing overlap with Munin's own cron jobs (which typically run on exact 5-minute marks).
#
#  Features:
#  - Configurable target server, user, and directory settings via external config file
#  - Efficient data transfer using rsync with --delete option
#  - Heartbeat file generation for external health monitoring
#  - Automatically skips remote sync when executed on the target server itself
#  - Local log directory is ensured before collecting system logs
#
#  Warning:
#  - This script updates files on a remote server under specified directories.
#  - Ensure proper SSH access and permissions are configured for the target.
#  - When run on the target server, sync operations are skipped to prevent redundant transfers.
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

# Determine if the script is running on the target server itself
is_target_server() {
    [ "$CURRENT_HOST" = "$TARGET_HOST" ] || echo "$SENDING" | grep -q "localhost"
}

# Load configuration from external file
load_config() {
    CURRENT_HOST=$(hostname -s)

    SCRIPT_DIR=$(dirname "$0")
    CONFIG_FILE="$SCRIPT_DIR/etc/munin-sync.conf"

    if [ ! -f "$CONFIG_FILE" ]; then
        CONFIG_FILE="$SCRIPT_DIR/../etc/munin-sync.conf"
    fi

    if [ -f "$CONFIG_FILE" ]; then
        . "$CONFIG_FILE"
    else
        echo "[ERROR] Configuration file not found: $CONFIG_FILE" >&2
        exit 1
    fi
}

# Sync Munin data to remote server
sync_munin_data() {
    if is_target_server; then
        #echo "[WARN] Running on target server. Skipping sync_munin_data." >&2
        return
    fi

    if [ ! -d "$MUNIN_DIR" ]; then
        echo "[ERROR] Munin directory not found: $MUNIN_DIR" >&2
        exit 1
    fi

    rsync $RSYNC_OPTS "$MUNIN_DIR" "$REMOTE_MUNIN_DIR"
}

# Ensure local log directory exists
ensure_log_dir() {
    [ -d "$LOG_DIR" ] || mkdir -p "$LOG_DIR"
}

# Sync log files to local directory
sync_local_logs() {
    rsync $RSYNC_OPTS /var/log/syslog "$LOG_DIR/"
    test -f /var/log/message && rsync $RSYNC_OPTS /var/log/message "$LOG_DIR/"
    rsync $RSYNC_OPTS /var/log/sysadmin/*.log "$LOG_DIR/"
    rsync $RSYNC_OPTS /var/log/deferred-sync/*.log "$LOG_DIR/"
    rsync $RSYNC_OPTS /var/log/clamav/clamscan.log "$LOG_DIR/"
    rsync $RSYNC_OPTS /var/log/chkrootkit/*.log "$LOG_DIR/"
}

# Create heartbeat file
create_heartbeat() {
    touch "$LOG_DIR/${SOURCE_HOST}_is_alive"
}

# Sync local logs to remote server
sync_logs_to_remote() {
    if is_target_server; then
        #echo "[WARN] Running on target server. Skipping sync_logs_to_remote." >&2
        return
    fi

    rsync $RSYNC_OPTS "$LOG_DIR" "$REMOTE_DIR"
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    load_config
    sync_munin_data
    ensure_log_dir
    sync_local_logs
    create_heartbeat
    sync_logs_to_remote
}

# Execute main function
main "$@"

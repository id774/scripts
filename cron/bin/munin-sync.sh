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
#  v1.0 2025-04-07
#       Initial release. Implements rsync-based data transfer and logging sync with heartbeat file generation.
#
#  Usage:
#      ./munin-sync.sh
#      This script is intended to be executed automatically via cron.
#
#  Cron Usage:
#  Add the following line to /etc/cron.d/munin-sync to execute every 5 minutes:
#      2-57/5 * * * * munin test -x /var/lib/munin/bin/munin-sync.sh && /var/lib/munin/bin/munin-sync.sh
#
#  This ensures the script runs every 5 minutes starting at the 2nd minute of each 5-minute interval,
#  reducing the chance of overlapping with Munin's own cron jobs which typically run on exact multiples of 5 minutes.
#
#  Features:
#  - POSIX-compliant script structure
#  - Full function-based structure with main() entry point
#  - Configurable target server, user, and directory settings via variables
#  - Efficient data transfer using rsync with delete option
#  - Heartbeat file generation for health monitoring
#  - Variable-based path configuration for flexibility
#
#  Warning:
#  - This script modifies the remote server by updating files under the specified directory.
#  - Use with caution and ensure proper permissions are set on the target server.
#
########################################################################

# VARIABLE DEFINITIONS
TARGET_USER="munin"
TARGET_HOST="REMOTE_SERVER_NAME"
TARGET_DIR="/home/share/received"
SOURCE_HOST="YOUR_HOST_NAME.id774.net"
MUNIN_CACHE_DIR="/var/cache/munin"
GROUP_NAME="id774.net"
SENT_LOGS="$HOME/sent_logs"
RSYNC_OPTS="-az --delete"
LOG_DIR="$SENT_LOGS/$SOURCE_HOST"
MUNIN_DIR="$MUNIN_CACHE_DIR/www/$GROUP_NAME/$SOURCE_HOST"
REMOTE_DIR="$TARGET_USER@$TARGET_HOST:$TARGET_DIR/"
REMOTE_MUNIN_DIR="$TARGET_USER@$TARGET_HOST:$MUNIN_CACHE_DIR/www/$GROUP_NAME/"

# Sync Munin data to remote server
sync_munin_data() {
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
    rsync $RSYNC_OPTS "$LOG_DIR" "$REMOTE_DIR"
}

# Main function to execute the script
main() {
    sync_munin_data
    ensure_log_dir
    sync_local_logs
    create_heartbeat
    sync_logs_to_remote
}

# Execute main function
main "$@"

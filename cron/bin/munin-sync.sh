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
#  Usage:
#      ./munin-sync.sh
#  This script is intended to be executed automatically via cron.
#
#  Cron Usage:
#  Add the following line to /etc/cron.d/munin-sync to execute every 5 minutes:
#      1-56/5 * * * * munin test -x /var/lib/munin/bin/munin-sync.sh && /var/lib/munin/bin/munin-sync.sh
#
#  This ensures the script runs every 5 minutes starting at the 2nd minute of each 5-minute interval,
#  reducing overlap with Munin's own cron jobs (which typically run on exact 5-minute marks).
#
#  Features:
#  - Configurable target server, user, and directory settings via external config file.
#  - Efficient data transfer using rsync with --delete option.
#  - Heartbeat file generation for external health monitoring.
#  - Automatically skips remote sync when executed on the target server itself.
#  - Local log directory is ensured before collecting system logs.
#
#  Warning:
#  - This script updates files on a remote server under specified directories.
#  - Ensure proper SSH access and permissions are configured for the target.
#  - When run on the target server, sync operations are skipped to prevent redundant transfers.
#
#  Version History:
#  v1.9 2025-09-20
#       Support multiple targets via TARGET_HOSTS and send to all specified hosts with self host.
#  v1.8 2025-09-19
#       Skip unreadable log files silently during local log collection to keep cron quiet.
#  v1.7 2025-08-03
#       Add clamav.log and its rotated file.
#  v1.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.5 2025-05-21
#       Modify sync_local_logs to include *.log.1 files while excluding *.log.2 and older logs.
#  v1.4 2025-05-16
#       Add return 0 to main and exit $? at script end for consistent exit status.
#  v1.3 2025-05-10
#       Added cron.log and Apache logs to local log synchronization.
#       Add cron execution check and usage support with unified structure.
#  v1.2 2025-04-14
#       Skip sync_munin_data and sync_logs_to_remote if script is running on the target server.
#  v1.1 2025-04-11
#       Added usage() function and --help option. Added Munin directory existence check.
#  v1.0 2025-04-07
#       Initial release. Implements rsync-based data transfer and logging sync with heartbeat file generation.
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

# Check if the script is running from cron
is_running_from_cron() {
    if tty -s; then
        return 1  # Terminal attached (interactive session)
    else
        return 0  # No terminal (likely cron)
    fi
}

# Determine if the given host represents this machine
is_self_host() {
    h="$1"
    # Normalize to short hostname to match $(hostname -s)
    h_short=${h%%.*}
    [ "$CURRENT_HOST" = "$h_short" ] || [ "$SENDING" = "localhost" ]
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

    # Normalize TARGET_HOSTS for backward compatibility
    case "x$TARGET_HOSTS" in
        x|"x ")
            #echo "[INFO] TARGET_HOSTS empty fallback to TARGET_HOST" >&2
            TARGET_HOSTS="$TARGET_HOST"
            ;;
        *)
            #echo "[INFO] Using multiple targets: $TARGET_HOSTS" >&2
            :
            ;;
    esac
}

# Sync Munin data to remote server
sync_munin_data() {
    if [ ! -d "$MUNIN_DIR" ]; then
        echo "[ERROR] Munin directory not found: $MUNIN_DIR" >&2
        exit 1
    fi

    for th in $TARGET_HOSTS; do
        if is_self_host "$th"; then
            #echo "[WARN] Running on target host $th. Skipping munin data sync to itself." >&2
            continue
        fi
        remote_munin_dir="$TARGET_USER@$th:$MUNIN_CACHE_DIR/www/$GROUP_NAME/"
        #echo "[INFO] Sync munin data to $remote_munin_dir" >&2
        rsync $RSYNC_OPTS "$MUNIN_DIR" "$remote_munin_dir"
    done
}

# Ensure local log directory exists
ensure_log_dir() {
    if [ ! -d "$LOG_DIR" ]; then
        mkdir -p "$LOG_DIR"
    fi
}

# Silently copy a file to $LOG_DIR if it is readable; otherwise skip without noise
copy_if_readable() {
    src="$1"
    dst_dir="$LOG_DIR"
    # Skip if src is missing or not readable
    if [ ! -r "$src" ]; then
        #echo "[WARN] Skip unreadable file: $src" >&2
        return 0
    fi
    #echo "[INFO] Copy $src to $dst_dir"
    # Delegate quietness to $RSYNC_OPTS (e.g. -q); ignore non-zero exit
    rsync $RSYNC_OPTS "$src" "$dst_dir/" || :
    return 0
}

# Sync log files to local directory
sync_local_logs() {
    # Sync individual log files if they are readable (skip silently otherwise)
    copy_if_readable /var/log/syslog
    copy_if_readable /var/log/syslog.1

    copy_if_readable /var/log/message
    copy_if_readable /var/log/message.1

    copy_if_readable /var/log/kern.log
    copy_if_readable /var/log/kern.log.1

    copy_if_readable /var/log/auth.log
    copy_if_readable /var/log/auth.log.1

    copy_if_readable /var/log/cron.log
    copy_if_readable /var/log/cron.log.1

    copy_if_readable /var/log/clamav/clamav.log
    copy_if_readable /var/log/clamav/clamav.log.1

    copy_if_readable /var/log/clamav/clamscan.log
    copy_if_readable /var/log/clamav/clamscan.log.1

    # Sync *.log and *.log.1 files from selected log directories safely
    for dir in /var/log/apache2 /var/log/sysadmin /var/log/deferred-sync /var/log/chkrootkit; do
        if [ -d "$dir" ]; then
            find "$dir" -type f | while IFS= read -r file; do
                case "$file" in
                    *.log|*.log.1)
                        # Only copy when readable; delegate quietness to $RSYNC_OPTS
                        if [ -r "$file" ]; then
                            #echo "[INFO] Copy $file to $LOG_DIR/"
                            rsync $RSYNC_OPTS "$file" "$LOG_DIR/" || :
                        else
                            #echo "[WARN] Skip unreadable file: $file" >&2
                            :
                        fi
                        ;;
                esac
            done
        fi
    done
}

# Create heartbeat file
create_heartbeat() {
    touch "$LOG_DIR/${SOURCE_HOST}_is_alive"
}

# Sync local logs to remote server
sync_logs_to_remote() {
    for th in $TARGET_HOSTS; do
        if is_self_host "$th"; then
            #echo "[INFO] Target is self host $th. Reflect logs locally." >&2
            # Place heartbeat and logs into local TARGET_DIR (no SSH)
            # Use trailing slash to copy contents of $LOG_DIR under $TARGET_DIR
            # shellcheck disable=SC2086
            rsync $RSYNC_OPTS "$LOG_DIR/" "$TARGET_DIR/" || :
            continue
        fi
        remote_dir="$TARGET_USER@$th:$TARGET_DIR/"
        #echo "[INFO] Sync logs to $remote_dir" >&2
        rsync $RSYNC_OPTS "$LOG_DIR" "$remote_dir"
    done
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    if ! is_running_from_cron; then
        echo "[ERROR] This script is intended to be run by cron only." >&2
        exit 1
    fi

    load_config
    sync_munin_data
    ensure_log_dir
    sync_local_logs
    create_heartbeat
    sync_logs_to_remote

    return 0
}

# Execute main function
main "$@"
exit $?

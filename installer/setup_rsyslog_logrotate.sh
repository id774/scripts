#!/bin/sh

########################################################################
# setup_rsyslog_logrotate.sh: Enforce rsyslog logrotate and journald limits
#
#  Description:
#    This script makes sure /etc/logrotate.d/rsyslog contains, inside
#    every brace block `{ ... }`, the three directives below with a
#    four-space indent:
#        "    daily"
#        "    rotate 90"
#        "    maxsize 100M"
#    If a block already has a frequency directive (daily/weekly/monthly/
#    yearly), it is normalized to "    daily". If a block already has
#    "rotate N", it is normalized to "    rotate 90". If a block already
#    has "maxsize N", it is normalized to "    maxsize 100M". If any of
#    these directives is missing, it is inserted immediately before the
#    closing brace.
#
#    In addition, the script manages a dedicated journald drop-in file:
#        /etc/systemd/journald.conf.d/99-storage-limits.conf
#    with the following content:
#        [Journal]
#        SystemMaxUse=500M
#        RuntimeMaxUse=200M
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./setup_rsyslog_logrotate.sh
#      ./setup_rsyslog_logrotate.sh -h   # prints this header
#
#  Why this approach:
#    * Idempotent: running the script multiple times does not keep
#      changing files once they are in the desired state.
#    * Safe metadata handling: updated content is copied back with cp -p
#      for rsyslog logrotate config to preserve mode/owner/timestamps.
#    * Minimal edits: only the target directives are touched; everything
#      else (delaycompress, postrotate, comments, ordering) is preserved.
#    * Separate journald policy: a dedicated drop-in file is managed so
#      the main journald.conf file remains untouched.
#    * Immediate effect: systemd-journald is restarted only when the
#      managed journald drop-in content actually changes.
#
#  Beginner notes:
#    * "logrotate" reads configuration files describing how to rotate
#      (rename and compress) logs. rsyslog writes system logs such as
#      /var/log/auth.log and /var/log/syslog. Placing "daily" reduces
#      each file’s size by rotating every day; "rotate 90" keeps 90
#      generations (roughly 90 days). "maxsize 100M" forces rotation
#      when the file exceeds 100MB.
#    * "SystemMaxUse=500M" limits how much disk space persistent journal
#      files under /var/log/journal may use in total.
#    * "RuntimeMaxUse=200M" limits how much temporary journal data under
#      /run/log/journal may use while the system is running.
#    * We first normalize tabs to four spaces so indentation looks
#      consistent across editors and diffs. Then we fix each block.
#    * Temporary files are written under /tmp and removed via a trap.
#
#  Ownership and mode policy:
#    * Owner and group: root:adm
#      - Debian based systems commonly use group "adm" for log reading and
#        auditing tasks. Assigning the config to root:adm allows controlled
#        group visibility for operators while keeping the file owned by root.
#    * Mode: 0640
#      - Drops world read permission to avoid leaking potentially sensitive
#        postrotate script fragments or custom paths in this file.
#      - Group readable for "adm" aligns with least privilege for on-host
#        troubleshooting without granting global access.
#    * /etc/logrotate.d/rsyslog
#      - Owner and group: root:adm
#      - Mode: 0640
#    * /etc/systemd/journald.conf.d/99-storage-limits.conf
#      - Owner and group: root:root
#      - Mode: 0644
#
#  Behavior summary:
#    1) If the rsyslog logrotate file contains any tab characters,
#       replace all tabs with four spaces (single pass).
#    2) In each brace block:
#         - normalize daily/weekly/monthly/yearly => "    daily"
#         - insert "    daily" if missing
#         - normalize "rotate N"                 => "    rotate 90"
#         - insert "    rotate 90" if missing
#         - normalize "maxsize N"               => "    maxsize 100M"
#         - insert "    maxsize 100M" if missing
#    3) Ensure the journald drop-in file contains the required
#       [Journal] limits.
#    4) Restart systemd-journald only when the drop-in content changed.
#    5) After changes, print the final files to the screen and a clear
#       concluding message. Also fix permissions on the logrotate status file.
#
#  Requirements:
#    - Linux
#    - sudo privileges (script uses sudo for reading/writing targets)
#    - Commands: sudo, awk, cp, cmp, grep, cat, rm, chown, chmod,
#      mkdir, systemctl, uname
#
#  Error Conditions:
#  0. Success.
#  1. General failure. (OS check, sudo, file missing, awk failure, etc.)
#  126. Required command(s) not executable.
#  127. Required command(s) not installed.
#
#  Version History:
#  v1.2 2026-04-04
#       Add journald drop-in management for SystemMaxUse and RuntimeMaxUse.
#  v1.1 2026-04-03
#       Add maxsize 100M enforcement for each logrotate block.
#  v1.0 2025-09-29
#       Initial release.
#
########################################################################

# Define managed paths
TARGET="/etc/logrotate.d/rsyslog"
JOURNALD_DIR="/etc/systemd/journald.conf.d"
JOURNALD_DROPIN="${JOURNALD_DIR}/99-storage-limits.conf"

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the system is Linux
check_system() {
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Verify the target file exists
check_target() {
    if ! sudo test -f "$TARGET"; then
        echo "[ERROR] $TARGET not found." >&2
        exit 1
    fi
}

# Remove temp files on normal exit or interruption
cleanup() {
    rm -f /tmp/rsyslog_tabs.$$ \
          /tmp/rsyslog_daily.$$ \
          /tmp/rsyslog_rotate.$$ \
          /tmp/rsyslog_maxsize.$$ \
          /tmp/journald_limits.$$
}
trap cleanup EXIT INT TERM

# Replace all tab characters with four spaces
# Notes:
#   - We only rewrite the file if at least one tab exists AND the content
#     actually changes. This avoids pointless churn and keeps timestamps.
convert_tabs() {
    if sudo grep -q "$(printf '\t')" "$TARGET" 2>/dev/null; then
        tmp="/tmp/rsyslog_tabs.$$"
        if ! sudo awk '{ gsub(/\t/, "    "); print }' "$TARGET" > "$tmp"; then
            rm -f "$tmp"
            echo "[ERROR] Failed to convert tabs." >&2
            exit 1
        fi
        if sudo cmp -s "$tmp" "$TARGET"; then
            rm -f "$tmp"                               # nothing to apply
        else
            if ! sudo cp -p "$tmp" "$TARGET"; then
                rm -f "$tmp"
                echo "[ERROR] Failed to write converted file." >&2
                exit 1
            fi
            rm -f "$tmp"
            echo "[INFO] Converted tabs to four spaces in $TARGET"
        fi
    fi
}

# Enforce ownership and permissions (reusable)
# Purpose:
#   Apply a consistent owner:group and mode to a given file path.
# Usage:
#   enforce_owner_mode <path> <owner> <group> <mode>
# Behavior:
#   - Fails fast on error, with actionable messages.
#   - Intended for both the rsyslog config and logrotate status files.
enforce_owner_mode() {
    path=$1
    owner=$2
    group=$3
    mode=$4

    if [ -z "$path" ] || [ -z "$owner" ] || [ -z "$group" ] || [ -z "$mode" ]; then
        echo "[ERROR] enforce_owner_mode requires: <path> <owner> <group> <mode>" >&2
        exit 1
    fi
    if ! sudo chown "$owner:$group" "$path"; then
        echo "[ERROR] Failed to set ownership ${owner}:${group} on $path" >&2
        exit 1
    fi
    if ! sudo chmod "$mode" "$path"; then
        echo "[ERROR] Failed to set mode $mode on $path" >&2
        exit 1
    fi
    echo "[INFO] Enforced ${owner}:${group} $mode on $path"
}

# Ensure the journald drop-in directory exists
ensure_journald_dir() {
    if ! sudo mkdir -p "$JOURNALD_DIR"; then
        echo "[ERROR] Failed to create $JOURNALD_DIR" >&2
        exit 1
    fi
}

# Ensure a frequency line "    daily" exists (and is normalized) in each block.
# Implementation details:
#   - Tracks when we are inside a block between "{" and "}".
#   - If a frequency line appears (daily/weekly/monthly/yearly), it is
#     replaced with "    daily" and the original is dropped.
#   - If we reach "}" without seeing a frequency line, we insert one
#     right before the closing brace.
#   - Trailing carriage returns are stripped to safely handle CRLF.
ensure_daily() {
    tmp="/tmp/rsyslog_daily.$$"
    if ! sudo awk '
        BEGIN { inblk=0; seen=0 }
        { sub(/\r$/, "") }                                  # strip CR if any
        /{[[:space:]]*$/ {                                  # enter block (matches lone "{" or " ... {")
            inblk=1; seen=0; print; next
        }
        inblk && /^[[:space:]]*(daily|weekly|monthly|yearly)[[:space:]]*(#.*)?$/ {
            if (seen==0) { print "    daily"; seen=1 }      # normalize frequency
            next                                            # drop original line
        }
        inblk && /^[[:space:]]*}[[:space:]]*$/ {            # leave block
            if (seen==0) { print "    daily" }              # insert if missing
            print; inblk=0; seen=0; next
        }
        { print }                                           # passthrough
    ' "$TARGET" > "$tmp"; then
        rm -f "$tmp"
        echo "[ERROR] Failed to process daily." >&2
        exit 1
    fi

    if sudo cmp -s "$tmp" "$TARGET"; then
        rm -f "$tmp"
        echo "[INFO] daily already set for all stanzas"
    else
        if ! sudo cp -p "$tmp" "$TARGET"; then
            rm -f "$tmp"
            echo "[ERROR] Failed to apply daily changes." >&2
            exit 1
        fi
        rm -f "$tmp"
        echo "[INFO] Set daily in one or more stanzas"
    fi

    # Enforce ownership and permissions after content normalization.
    # Rationale:
    # - Keep the file under root control while allowing audited group
    #   visibility via adm and drop world readability.
    # - Run each pass to stay idempotent and self healing if external
    #   tools downgraded permissions between runs.
    # Error handling:
    # - Fail fast if either ownership or permission cannot be applied.
    #   This prevents proceeding with a misconfigured policy.
    enforce_owner_mode "$TARGET" root adm 0640
}

# Ensure a retention line "    rotate 90" exists (and is normalized) in each block.
# Implementation details:
#   - Similar to ensure_daily, but targets "rotate <number>" lines.
#   - If a rotate line exists, we replace it with "    rotate 90".
#   - If missing when "}" is reached, we insert "    rotate 90" before it.
ensure_rotate90() {
    tmp="/tmp/rsyslog_rotate.$$"
    if ! sudo awk '
        BEGIN { inblk=0; seen=0 }
        { sub(/\r$/, "") }                                  # strip CR if any
        /{[[:space:]]*$/ {                                  # enter block (matches lone "{" or " ... {")
            inblk=1; seen=0; print; next
        }
        inblk && /^[[:space:]]*rotate[[:space:]]+[0-9]+[[:space:]]*(#.*)?$/ {
            if (seen==0) { print "    rotate 90"; seen=1 }  # normalize rotate
            next                                            # drop original line
        }
        inblk && /^[[:space:]]*}[[:space:]]*$/ {            # leave block
            if (seen==0) { print "    rotate 90" }          # insert if missing
            print; inblk=0; seen=0; next
        }
        { print }                                           # passthrough
    ' "$TARGET" > "$tmp"; then
        rm -f "$tmp"
        echo "[ERROR] Failed to process rotate." >&2
        exit 1
    fi

    if sudo cmp -s "$tmp" "$TARGET"; then
        rm -f "$tmp"
        echo "[INFO] rotate 90 already set for all stanzas"
    else
        if ! sudo cp -p "$tmp" "$TARGET"; then
            rm -f "$tmp"
            echo "[ERROR] Failed to apply rotate changes." >&2
            exit 1
        fi
        rm -f "$tmp"
        echo "[INFO] Set rotate 90 in one or more stanzas"
    fi

    # Reassert the same policy here too.
    # Why again:
    # - Each major normalization step ends by enforcing policy so the
    #   script remains robust even if intermediate edits changed it.
    # Security note:
    # - 0640 is sufficient because logrotate runs as root and adm can
    #   read for on host audits while world access stays disabled.
    # Error handling mirrors the daily step and fails fast on errors.
    enforce_owner_mode "$TARGET" root adm 0640
}

# Ensure a size limit line "    maxsize 100M" exists (and is normalized) in each block
ensure_maxsize() {
    tmp="/tmp/rsyslog_maxsize.$$"
    if ! sudo awk '
        BEGIN { inblk=0; seen=0 }
        { sub(/\r$/, "") }
        /{[[:space:]]*$/ {
            inblk=1; seen=0; print; next
        }
        inblk && /^[[:space:]]*maxsize[[:space:]]+[0-9]+[kKmMgG]?[[:space:]]*(#.*)?$/ {
            if (seen==0) { print "    maxsize 100M"; seen=1 }
            next
        }
        inblk && /^[[:space:]]*}[[:space:]]*$/ {
            if (seen==0) { print "    maxsize 100M" }
            print; inblk=0; seen=0; next
        }
        { print }
    ' "$TARGET" > "$tmp"; then
        rm -f "$tmp"
        echo "[ERROR] Failed to process maxsize." >&2
        exit 1
    fi

    if sudo cmp -s "$tmp" "$TARGET"; then
        rm -f "$tmp"
        echo "[INFO] maxsize already set for all stanzas"
    else
        if ! sudo cp -p "$tmp" "$TARGET"; then
            rm -f "$tmp"
            echo "[ERROR] Failed to apply maxsize changes." >&2
            exit 1
        fi
        rm -f "$tmp"
        echo "[INFO] Set maxsize 100M in one or more stanzas"
    fi

    enforce_owner_mode "$TARGET" root adm 0640
}

# Ensure the journald drop-in file contains the desired limits
# Return codes:
#   0: content updated
#   1: already in desired state
ensure_journald_limits() {
    tmp="/tmp/journald_limits.$$"

    # Build the desired drop-in content in a temporary file first
    cat > "$tmp" <<'EOF'
[Journal]
SystemMaxUse=500M
RuntimeMaxUse=200M
EOF

    if sudo test -f "$JOURNALD_DROPIN" && sudo cmp -s "$tmp" "$JOURNALD_DROPIN"; then
        rm -f "$tmp"
        echo "[INFO] journald limits already configured"
        return 1
    fi

    if ! sudo cp "$tmp" "$JOURNALD_DROPIN"; then
        rm -f "$tmp"
        echo "[ERROR] Failed to write $JOURNALD_DROPIN" >&2
        exit 1
    fi
    rm -f "$tmp"

    enforce_owner_mode "$JOURNALD_DROPIN" root root 0644
    echo "[INFO] Updated $JOURNALD_DROPIN"
    return 0
}

# Restart journald only after the drop-in content changed
restart_journald() {
    if ! sudo systemctl restart systemd-journald; then
        echo "[ERROR] Failed to restart systemd-journald" >&2
        exit 1
    fi
    echo "[INFO] Restarted systemd-journald"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands sudo awk cp cmp grep cat rm chown chmod mkdir systemctl
    check_sudo
    check_target
    ensure_journald_dir

    convert_tabs
    ensure_daily
    ensure_rotate90
    ensure_maxsize

    # Update journald limits only when needed
    if ensure_journald_limits; then
        restart_journald
    fi

    # Normalize logrotate status file as well
    enforce_owner_mode /var/lib/logrotate/status root adm 0640

    # Show the final files for quick human verification
    echo "----- BEGIN $TARGET -----"
    sudo cat "$TARGET"
    echo "----- END $TARGET -----"

    echo "----- BEGIN $JOURNALD_DROPIN -----"
    sudo cat "$JOURNALD_DROPIN"
    echo "----- END $JOURNALD_DROPIN -----"

    echo "[INFO] This is the final result"
    return 0
}

# Execute main function
main "$@"

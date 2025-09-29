#!/bin/sh

########################################################################
# setup_rsyslog_logrotate.sh: Enforce daily and rotate 90 for rsyslog
#
#  Description:
#    This script makes sure /etc/logrotate.d/rsyslog contains, inside
#    every brace block `{ ... }`, the two directives below with a
#    four-space indent:
#        "    daily"
#        "    rotate 90"
#    If a block already has a frequency directive (daily/weekly/monthly/
#    yearly), it is normalized to "    daily". If a block already has
#    "rotate N", it is normalized to "    rotate 90". If either directive
#    is missing, it is inserted immediately before the closing brace.
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
#      changing the file once it is in the desired state.
#    * Safe metadata handling: updated content is copied back with cp -p
#      to preserve mode/owner/timestamps.
#    * Minimal edits: only the target directives are touched; everything
#      else (delaycompress, postrotate, comments, ordering) is preserved.
#
#  Beginner notes:
#    * "logrotate" reads configuration files describing how to rotate
#      (rename and compress) logs. rsyslog writes system logs such as
#      /var/log/auth.log and /var/log/syslog. Placing "daily" reduces
#      each file’s size by rotating every day; "rotate 90" keeps 90
#      generations (roughly 90 days).
#    * We first normalize tabs to four spaces so indentation looks
#      consistent across editors and diffs. Then we fix each block.
#    * Temporary files are written under /tmp and removed via a trap.
#
#  Behavior summary:
#    1) If the file contains any tab characters, replace all tabs with
#       four spaces (single pass).
#    2) In each brace block:
#         - normalize daily/weekly/monthly/yearly => "    daily"
#         - insert "    daily" if missing
#         - normalize "rotate N"                 => "    rotate 90"
#         - insert "    rotate 90" if missing
#    3) After changes, print the final file to the screen and a clear
#       concluding message.
#
#  Requirements:
#    - Linux
#    - sudo privileges (script uses sudo for reading/writing the target)
#    - Commands: sudo, awk, cp, cmp, grep, cat, rm
#
#  Error Conditions:
#  0. Success
#  1. General failure (OS check, sudo, file missing, awk failure, etc.)
#  126. Required command(s) not executable.
#  127. Required command(s) not installed.
#
#  Version History:
#  v1.0 2025-09-29
#       Initial release.
#
########################################################################

# Target configuration file
TARGET="/etc/logrotate.d/rsyslog"

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
    rm -f /tmp/rsyslog_tabs.$$ /tmp/rsyslog_daily.$$ /tmp/rsyslog_rotate.$$
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

# Ensure a frequency line "    daily" exists (and is normalized) in each block
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
}

# Ensure a retention line "    rotate 90" exists (and is normalized) in each block
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
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands sudo awk cp cmp grep cat rm
    check_sudo
    check_target

    convert_tabs
    ensure_daily
    ensure_rotate90

    # Final verification output helps humans confirm the result quickly
    echo "----- BEGIN $TARGET -----"
    sudo cat "$TARGET"
    echo "----- END $TARGET -----"
    echo "[INFO] This is the final result"
    return 0
}

# Execute main function
main "$@"
exit $?

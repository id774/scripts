#!/bin/sh

########################################################################
# get-mountpoint.sh: Resolve mountpoint from a block device
#
#  Description:
#  This utility takes a block device path (e.g. /dev/sdc, /dev/sdc1,
#  /dev/mapper/truecrypt1, /dev/dm-0) and determines the mountpoint
#  where it is currently mounted. It prints a single mountpoint path
#  to stdout, making it suitable for both interactive checks and
#  automation scripts.
#
#  Features:
#    - Accepts base disks, partitions, and device-mapper paths.
#    - Searches both the device itself and its ancestors/descendants
#      and returns the best matching mountpoint deterministically.
#    - Uses findmnt(8) primarily; falls back to lsblk(8) scanning.
#    - Handles MOUNTPOINTS column used by newer util-linux.
#    - Deterministic exit codes for robust scripting.
#
#  Notes:
#    - Designed for Linux systems with lsblk(8) and findmnt(8) available.
#    - Selection policy for multiple candidates:
#        A) Prefer the candidate with the greatest dependency depth
#           from the base device (deepest descendant).
#        B) Tie breaker 1: prefer root mountpoint "/".
#        C) Tie breaker 2: prefer non-removable-like FS over vfat/msdos/exfat/iso9660/squashfs.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      get-mountpoint.sh <device>
#    Examples:
#      get-mountpoint.sh /dev/sdc1
#      => /mnt/disk1
#      get-mountpoint.sh /dev/sde
#      => /            # when the deepest descendant mounts "/"
#
#  Error Conditions:
#  0. Success.
#  1. General failure.
#  2. Device not found or not mounted.
#  3. Path is not a block device.
#  126. Required command is not executable.
#  127. Required command is not installed.
#
#  Version History:
#  v1.0 2025-08-31
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

# Print error and exit with code
fail() {
    code="$1"; shift
    echo "[ERROR] $*" >&2
    exit "$code"
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

# Validate CLI arguments and device existence
validate_args() {
    if [ $# -ne 1 ]; then
        echo "[ERROR] Exactly one device argument is required." >&2
        exit 2
    fi

    DEV="$1"

    # Allow symlinks like /dev/disk/by-id/...; ensure it exists
    if [ ! -e "$DEV" ]; then
        echo "[ERROR] Device path does not exist: $DEV" >&2
        exit 2
    fi

    # Require a block device (regular file paths are not accepted)
    if [ ! -b "$DEV" ]; then
        echo "[ERROR] Not a block device: $DEV" >&2
        exit 3
    fi

    DEVICE="$DEV"
    return 0
}

# Try to obtain mountpoint directly from the device
find_mountpoint_direct() {
    SRC="$1"
    MP=$(findmnt -nr -S "$SRC" -o TARGET 2>/dev/null || true)
    if [ -n "$MP" ]; then
        printf '%s\n' "$MP"
        return 0
    fi
    return 1
}

# List dependency chain ancestors for a device (upwards to base)
# and descendants (children) for a base disk. Output: one NAME per line.
list_related_devices() {
    DEVPATH="$1"
    lsblk -rnp -o NAME -s -- "$DEVPATH" 2>/dev/null
    lsblk -rnp -o NAME    -- "$DEVPATH" 2>/dev/null
}

# Parse NAME,FSTYPE,MOUNTPOINTS and return NAME<TAB>FSTYPE<TAB>FIRST_MOUNTPOINT
get_name_fstype_first_mp() {
    lsblk -rpn -o NAME,FSTYPE,MOUNTPOINTS -P -- "$1" 2>/dev/null | awk '
        {
            nm=""; mp=""; fs="";
            for(i=1;i<=NF;i++){
                if ($i ~ /^NAME="/)         nm=$i;
                else if ($i ~ /^FSTYPE="/)  fs=$i;
                else if ($i ~ /^MOUNTPOINTS="/) mp=$i;
            }
            if (nm != "" && mp != "") {
                gsub(/^[^=]*="/,"",nm); gsub(/"$/,"",nm);
                gsub(/^[^=]*="/,"",mp); gsub(/"$/,"",mp);
                n=split(mp, arr, /[[:space:]]+/);
                if (n>=1) {
                    gsub(/^[^=]*="/,"",fs); gsub(/"$/,"",fs);
                    print nm "\t" fs "\t" arr[1];
                }
            }
        }'
}

# Compute depth from BASE to CAND in the block dependency chain
# returns integer >=0 when CAND descends from BASE, otherwise -1
depth_from_base() {
    base="$1"; cand="$2"
    d=$(lsblk -rpn -o NAME -s -- "$cand" 2>/dev/null | awk -v b="$base" '{
        if ($0==b){ print NR-1; exit }
    }')
    if [ -n "$d" ]; then
        printf '%s\n' "$d"
    else
        printf '%s\n' "-1"
    fi
}

# Fallback: scan lsblk NAME and MOUNTPOINTS columns and return first non-empty mountpoint
scan_mountpoint_via_lsblk() {
    DEVPATH="$1"
    lsblk -rpn -o NAME,TYPE,MOUNTPOINTS,MOUNTPOINT -P -- "$DEVPATH" 2>/dev/null | awk '
        {
            mp="";
            for(i=1;i<=NF;i++){
                if ($i ~ /^MOUNTPOINTS="/) mp=$i;
                else if ($i ~ /^MOUNTPOINT="/ && mp=="") mp=$i;
            }
            if (mp != "") {
                gsub(/^[^=]*="/,"",mp); gsub(/"$/,"",mp);
                n=split(mp, arr, /[[:space:]]+/);
                if (n>=1) { print arr[1]; exit }
            }
        }'
}

# Resolve and print mountpoint with error handling
resolve_and_print() {
    DEV="$1"

    # 1) Try direct match for the given device
    MP=$(find_mountpoint_direct "$DEV") && { printf '%s\n' "$MP"; return 0; }

    # 2) Build candidate list from ancestors and descendants (no subshell leaks)
    tmpfile=""
    if tmpfile=$(mktemp 2>/dev/null); then :; else fail 1 "Failed to create temporary file"; fi
    list_related_devices "$DEV" | awk 'NF>0' > "$tmpfile"

    candfile=""
    if candfile=$(mktemp 2>/dev/null); then
        :
    else
        rm -f "$tmpfile"
        fail 1 "Failed to create temporary file"
    fi

    seen=""
    while IFS= read -r CAND; do
        case " $seen " in *" $CAND "*) continue ;; *) seen="$seen $CAND" ;; esac

        MP=$(find_mountpoint_direct "$CAND")
        if [ -n "$MP" ]; then
            FS=$(findmnt -nr -S "$CAND" -o FSTYPE 2>/dev/null || true)
            [ -n "$FS" ] || FS=""
            printf '%s\t%s\t%s\n' "$CAND" "$FS" "$MP" >> "$candfile"
            continue
        fi

        get_name_fstype_first_mp "$CAND" >> "$candfile"
    done < "$tmpfile"
    rm -f "$tmpfile"

    # 3) Select best candidate by policy (depth > root > non-vfat)
    best_mp=""
    best_depth="-1"
    best_fs=""

    while IFS= read -r line; do
        # Extract fields robustly regardless of IFS/tab handling
        CNAME=$(printf '%s\n' "$line" | awk -F '\t' '{print $1}')
        CFS=$(    printf '%s\n' "$line" | awk -F '\t' '{print $2}')
        CMP=$(    printf '%s\n' "$line" | awk -F '\t' '{print $3}')
        [ -n "$CMP" ] || continue

        d=$(depth_from_base "$DEV" "$CNAME")
        [ "$d" -ge 0 ] || continue

        if [ "$best_depth" = "-1" ]; then
            best_depth="$d"; best_mp="$CMP"; best_fs="$CFS"
            continue
        fi

        if [ "$d" -gt "$best_depth" ]; then
            best_depth="$d"; best_mp="$CMP"; best_fs="$CFS"
            continue
        fi

        if [ "$d" -eq "$best_depth" ]; then
            if [ "$CMP" = "/" ] && [ "$best_mp" != "/" ]; then
                best_mp="$CMP"; best_fs="$CFS"
                continue
            fi
            case "$best_fs" in vfat|msdos|exfat|iso9660|squashfs) best_is_vfat=1 ;; *) best_is_vfat=0 ;; esac
            case "$CFS"         in vfat|msdos|exfat|iso9660|squashfs) cur_is_vfat=1  ;; *) cur_is_vfat=0  ;; esac
            if [ "$best_is_vfat" -eq 1 ] && [ "$cur_is_vfat" -eq 0 ]; then
                best_mp="$CMP"; best_fs="$CFS"
                continue
            fi
        fi
    done < "$candfile"

    rm -f "$candfile"

    if [ -n "$best_mp" ]; then
        printf '%s\n' "$best_mp"
        return 0
    fi

    # 4) Last fallback via lsblk scan
    MP=$(scan_mountpoint_via_lsblk "$DEV")
    if [ -n "$MP" ]; then
        printf '%s\n' "$MP"
        return 0
    fi

    fail 2 "Device not mounted or no mounted children found: $DEV"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands lsblk findmnt awk sed
    validate_args "$@"
    resolve_and_print "$DEVICE"
    return $?
}

# Execute main function
main "$@"
exit $?

#!/bin/sh

########################################################################
# setup_rsyslog_postfix.sh: Deploy postfix-only routing rule for rsyslog
#
#  Description:
#  Deploy a drop-in rsyslog configuration that routes all postfix-tagged
#  messages exclusively to /var/log/postfix.log and stops further
#  processing to avoid duplication in syslog.
#
#  The source file is expected at:
#      $SCRIPTS/etc/rsyslog.d/30-postfix.conf
#  The target location is:
#      /etc/rsyslog.d/30-postfix.conf
#
#  Behavior:
#    - Idempotent deployment: only writes when missing or content differs.
#    - Pre-checks if an equivalent split already exists and skips if so.
#    - Validates rsyslog config with 'rsyslogd -N1' and restarts the service.
#
#  Example content of 30-postfix.conf (rsyslog v8 property-based filter):
#      # Route postfix to a dedicated log and stop further processing
#      :syslogtag, startswith, "postfix/" /var/log/postfix.log
#      & stop
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./setup_rsyslog_postfix.sh
#  Run as a regular user with sudo privileges; the script will invoke sudo
#  where necessary.
#
#  Notes:
#  - No backups are created; deployment is applied directly when needed.
#  - The environment variable SCRIPTS must point to the root of this repo.
#  - The source file must exist at $SCRIPTS/etc/rsyslog.d/30-postfix.conf.
#  - Designed for Debian/Ubuntu systems running rsyslog v8.
#
#  Requirements:
#  - Linux operating system
#  - sudo privileges for writing under /etc/rsyslog.d and controlling rsyslog
#  - Commands: sudo, awk, find, grep, cmp, chown, chmod, cp, mktemp, rsyslogd, uname, test
#
#  Version History:
#  v1.0 2025-08-26
#       Initial release.
#
########################################################################

TARGET_DIR="/etc/rsyslog.d"
TARGET_FILE="$TARGET_DIR/30-postfix.conf"

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

# Check if the SCRIPTS variable is unset or empty
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the path of your script collection." >&2
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

# Ensure SCRIPTS is set and source file exists
check_source_file() {
    SRC_FILE="$SCRIPTS/etc/rsyslog.d/30-postfix.conf"
    if [ ! -f "$SRC_FILE" ]; then
        echo "[ERROR] Source file not found: $SRC_FILE" >&2
        exit 1
    fi
}

# Ensure target directory exists
check_target_dir() {
    if ! sudo test -d "$TARGET_DIR"; then
        echo "[ERROR] $TARGET_DIR does not exist. Is rsyslog installed?" >&2
        exit 1
    fi
}

# Detect existing postfix-only split logging rules to avoid duplication
has_postfix_split() {
    # Consider both main file and common include directories
    files_main="/etc/rsyslog.conf"
    files_dropin=$(find /etc/rsyslog.conf.d "$TARGET_DIR" -maxdepth 1 -type f -name '*.conf' 2>/dev/null)

    # Heuristics:
    #  - Any rule that routes postfix to a dedicated file (e.g., /var/log/postfix.log)
    #  - Or a property filter explicitly matching postfix syslogtag/programname with a stop
    #  - Skip commented lines
    grep -hE '^[[:space:]]*[^#].*(/var/log/postfix\.log|syslogtag.*postfix/|programname.*postfix)' \
        $files_main $files_dropin 2>/dev/null | grep -q .
}

# Deploy the config if needed (content differs or file absent)
deploy_conf() {
    SRC_FILE="$SCRIPTS/etc/rsyslog.d/30-postfix.conf"
    TMP_FILE="$(mktemp /tmp/setup_rsyslog_postfix.XXXXXX)" || exit 1

    cp "$SRC_FILE" "$TMP_FILE"

    # Compare with existing target
    if sudo test -f "$TARGET_FILE"; then
        if sudo cmp -s "$TMP_FILE" "$TARGET_FILE"; then
            rm -f "$TMP_FILE"
            echo "[INFO] $TARGET_FILE is already up to date. No changes."
            return 0
        fi
    fi

    # Install with correct permissions
    if sudo cp "$TMP_FILE" "$TARGET_FILE"; then
        echo "[INFO] Deployed $TARGET_FILE"
        sudo chown root:root "$TARGET_FILE"
        sudo chmod 0644 "$TARGET_FILE"
    else
        echo "[ERROR] Failed to install $TARGET_FILE" >&2
        rm -f "$TMP_FILE"
        exit 1
    fi
    rm -f "$TMP_FILE"
}

# Validate rsyslog configuration and restart service
validate_and_restart() {
    if sudo rsyslogd -N1 >/dev/null 2>&1; then
        echo "[INFO] rsyslog config validation OK"
    else
        echo "[ERROR] rsyslog config validation failed" >&2
        sudo rsyslogd -N1 || true
        exit 1
    fi

    if command -v systemctl >/dev/null 2>&1; then
        sudo systemctl restart rsyslog || {
            echo "[WARN] systemctl restart failed; trying legacy service manager" >&2
            sudo service rsyslog restart 2>/dev/null || sudo /etc/init.d/rsyslog restart 2>/dev/null || true
        }
    else
        sudo service rsyslog restart 2>/dev/null || sudo /etc/init.d/rsyslog restart 2>/dev/null || true
    fi
    echo "[INFO] rsyslog restarted"
}

# Decide whether to deploy and restart based on existing configs
maybe_deploy_and_restart() {
    if has_postfix_split; then
        echo "[INFO] Existing config already splits postfix logs."
        echo "[INFO] Skipping deployment of $TARGET_FILE."
    else
        deploy_conf
        validate_and_restart
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_scripts
    check_commands awk find grep cmp chown chmod cp mktemp rsyslogd uname
    check_sudo
    check_source_file
    check_target_dir

    maybe_deploy_and_restart
    return 0
}

# Execute main function
main "$@"
exit $?

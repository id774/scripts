#!/bin/sh

########################################################################
# fix-permissions.sh: Fix File and Directory Permissions
#
#  Description:
#  This script adjusts the permissions and ownership of critical system
#  directories and files to ensure they are set correctly. It logs all
#  operations and sends the log file to the administrator via email.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.5 2025-05-17
#       Refactor to load settings and operations from external fix-permissions.conf.
#       Separated configuration from script logic for host-specific customization.
#  v1.4 2025-05-16
#       Add return 0 to main and exit $? at script end for consistent exit status.
#  v1.3 2025-05-11
#       Restrict cron file permissions by removing all rights from others,
#       allowing only read for group, and changing group ownership to adm.
#  v1.2 2025-05-10
#       Refactor into function-based, POSIX-compliant structure with cron check and usage display.
#  v1.1 2025-03-19
#       Improved POSIX compliance, standardized redirections, and enhanced readability.
#  v1.0 2024-12-09
#       Initial release with logging, permission adjustments, and email reporting.
#
#  Usage:
#  Execute the script directly without any arguments:
#      ./fix-permissions.sh
#      This script is intended to be executed periodically by cron.
#
#  Features:
#  - Recursively changes ownership of specific directories to root.
#  - Sets appropriate permissions for cron directories and files.
#  - Logs all operations with timestamps and host details.
#  - Sends a summary of operations to a specified admin email.
#
#  Requirements:
#  - Must be executed with sufficient permissions to change ownership
#    and modify file attributes (typically as root).
#  - The `mail` command must be installed and configured to send emails.
#  - Ensure the `nkf` command is installed for UTF-8 email conversion.
#
#  Notes:
#  - The `ADMIN_MAIL_ADDRESS` environment variable can be set to specify
#    a recipient email address. Default is `root`.
#  - Logs are written to `/var/log/sysadmin/fix-permissions.log`.
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

# Function to check if the script is running from cron
is_running_from_cron() {
    if tty -s; then
        return 1  # Terminal attached (interactive session)
    else
        return 0  # No terminal (likely cron)
    fi
}

# Check if JOBLOG is writable
is_joblog_writable() {
    if [ ! -w "$JOBLOG" ] && ! touch "$JOBLOG" 2>/dev/null; then
        echo "[ERROR] JOBLOG is not writable: $JOBLOG" >&2
        exit 1
    fi
}

# Initialize environment and log start
initialize() {
    LC_CTYPE=ja_JP.UTF-8
    JOBLOG="/var/log/sysadmin/fix-permissions.log"

    if ! is_running_from_cron; then
        echo "[ERROR] This script is intended to be run by cron only." >&2
        exit 1
    fi

    is_joblog_writable

    echo -n "*** $0: Job started on $(hostname) at " >> "$JOBLOG" 2>&1
    date "+%Y/%m/%d %T" >> "$JOBLOG" 2>&1
}

# Finalize job and log completion
finalize() {
    echo -n "*** $0: Job ended on $(hostname) at " >> "$JOBLOG" 2>&1
    date "+%Y/%m/%d %T" >> "$JOBLOG" 2>&1
    echo >> "$JOBLOG" 2>&1
}

# Optionally send the job log via email
send_mail_to_admin() {
    if [ -n "$ADMIN_MAIL_ADDRESS" ]; then
        if [ -r "$JOBLOG" ]; then
            cat -v "$JOBLOG" | nkf -w | mail -s "[cron][$(hostname)] Fixed Permissions Log" "$ADMIN_MAIL_ADDRESS"
        fi
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    initialize

    SCRIPT_NAME="$(basename "$0")"
    CONFIG_FILE="/etc/cron.config/${SCRIPT_NAME}.conf"

    if [ -f "$CONFIG_FILE" ]; then
        echo "[INFO] Loaded configuration from $CONFIG_FILE." >> "$JOBLOG" 2>&1
        . "$CONFIG_FILE"
    else
        echo "[ERROR] Configuration file not found: $CONFIG_FILE" >> "$JOBLOG" 2>&1
        exit 9
    fi

    fix_permissions
    finalize

    send_mail_to_admin

    return 0
}

# Execute main function
main "$@"
exit $?

#!/bin/sh

########################################################################
# restorecon.sh: Restore SELinux Security Contexts
#
#  Description:
#  This script restores SELinux security contexts for key system directories.
#  - Ensures SELinux is enabled before execution.
#  - Validates that the required commands exist.
#  - Runs `restorecon` with verbose and recursive options on essential paths.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-05-16
#       Add return 0 to main and exit $? at script end for consistent exit status.
#  v1.2 2025-05-10
#       Add cron execution check and usage support with unified structure.
#  v1.1 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.0 2025-03-16
#       Added Linux OS check, SELinux status validation, and command existence check.
#       Improved safety with error handling and sudo enforcement.
#  v0.1 2014-06-02
#       Initial version.
#
#  Usage:
#      ./restorecon.sh
#
#  Requirements:
#  - Must be executed on a Linux system with SELinux enabled.
#  - Requires `restorecon` and `sestatus` commands to be available.
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

# Function to check if SELinux is enabled
check_selinux() {
    if ! sestatus 2>/dev/null | grep -q "enabled"; then
        echo "[ERROR] SELinux is not enabled. This script will not proceed." >&2
        exit 1
    fi
}

# Function to restore SELinux contexts
restore_selinux_context() {
    echo "Restoring SELinux security contexts..."
    for dir in /usr /opt /etc /var /root /home; do
        echo "Executing: restorecon on $dir"
        restorecon -RFv "$dir"
    done
    echo "SELinux context restoration completed."
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    if ! is_running_from_cron; then
        echo "[ERROR] This script is intended to be run by cron only." >&2
        exit 1
    fi

    check_selinux
    restore_selinux_context

    return 0
}

# Execute main function
main "$@"
exit $?

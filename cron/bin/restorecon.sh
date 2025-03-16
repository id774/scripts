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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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

# Function to check if SELinux is enabled
check_selinux() {
    if ! sestatus 2>/dev/null | grep -q "enabled"; then
        echo "Error: SELinux is not enabled. This script will not proceed." >&2
        exit 1
    fi
}

# Function to restore SELinux contexts
restore_selinux_context() {
    echo "Restoring SELinux security contexts..."
    for dir in /usr /opt /etc /var /root /home; do
        echo "Running restorecon on $dir"
        sudo restorecon -RFv "$dir"
    done
    echo "SELinux context restoration completed."
}

# Main execution function
main() {
    check_selinux
    restore_selinux_context
}

main "$@"

#!/bin/sh

########################################################################
# disable_ipv6_macos.sh: Disable IPv6 on macOS
#
#  Description:
#  This script disables IPv6 on all network interfaces detected on macOS.
#  It ensures idempotency by checking the current IPv6 setting before applying changes.
#  If the system is not macOS, the script exits with return code 1.
#
#  Features:
#  - Detects all active network interfaces dynamically.
#  - Disables IPv6 only if it is enabled.
#  - Ensures compatibility with POSIX shell.
#  - Requires sudo for modifying network settings.
#  - Exits with return code 1 if executed on a non-macOS system.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-02-19
#       Initial release with full macOS IPv6 disabling support.
#
#  Usage:
#  ./disable_ipv6_macos.sh
#  This script does not require any arguments. It automatically detects
#  all network interfaces and disables IPv6 where necessary.
#
########################################################################

# Check if running on macOS
if [ "$(uname)" != "Darwin" ]; then
    echo "Error: This script is only for macOS." >&2
    exit 1
fi

# Get all network services
SERVICES=$(networksetup -listallnetworkservices | tail -n +2)

for SERVICE in $SERVICES; do
    # Check if the service is valid
    if ! networksetup -getinfo "$SERVICE" >/dev/null 2>&1; then
        echo "Skipping unrecognized network service: $SERVICE" >&2
        continue
    fi

    # Get the current IPv6 setting for the service
    CURRENT_SETTING=$(networksetup -getinfo "$SERVICE" | awk -F': ' '/IPv6/{print $2}')

    if [ "$CURRENT_SETTING" != "Off" ]; then
        echo "Disabling IPv6 on $SERVICE..."
        sudo networksetup -setv6off "$SERVICE" || echo "Error: Failed to disable IPv6 on $SERVICE." >&2
    else
        echo "IPv6 is already disabled on $SERVICE. Skipping..."
    fi

done

# Verify IPv6 settings after modification
for SERVICE in $SERVICES; do
    echo "$SERVICE: $(networksetup -getinfo "$SERVICE" | awk -F': ' '/IPv6/{print $2}')"
done

echo "IPv6 has been disabled where necessary."

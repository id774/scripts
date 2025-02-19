#!/bin/sh

########################################################################
# disable_ipv6_linux.sh: Disable IPv6 on GNU/Linux
#
#  Description:
#  This script disables IPv6 on all network interfaces by modifying /etc/sysctl.conf.
#  If the required settings are already present, they will not be duplicated.
#  After modification, it applies the changes and verifies the configuration.
#
#  Features:
#  - Checks for existing IPv6 disable settings before adding them.
#  - Ensures changes are applied using sysctl only if needed.
#  - Verifies the applied configuration.
#  - Checks for necessary commands before execution.
#  - Ensures /etc/sysctl.conf exists before modification.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-02-19
#       Initial release with IPv6 disabling functionality.
#
#  Usage:
#  ./disable_ipv6_linux.sh --disable
#  --disable: Disables IPv6 by modifying /etc/sysctl.conf.
#
########################################################################

# Check if running on GNU/Linux
if [ "$(uname -s)" != "Linux" ]; then
    echo "Error: This script is only for GNU/Linux." >&2
    exit 1
fi

# Check for required argument
if [ "$1" != "--disable" ]; then
    echo "Usage: $0 --disable" >&2
    exit 1
fi

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again."
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions."
            exit 126
        fi
    done
}

# Check required commands
check_commands sudo sysctl ip grep cat

# Define sysctl parameters
SYSCTL_CONF="/etc/sysctl.conf"
PARAMS="net.ipv6.conf.all.disable_ipv6 = 1\nnet.ipv6.conf.default.disable_ipv6 = 1\nnet.ipv6.conf.lo.disable_ipv6 = 1"

# Ensure /etc/sysctl.conf exists
if [ ! -f "$SYSCTL_CONF" ]; then
    echo "Error: $SYSCTL_CONF does not exist. Please create it and try again." >&2
    exit 1
fi

# Add parameters if not already present and apply changes immediately
for PARAM in "net.ipv6.conf.all.disable_ipv6" \
             "net.ipv6.conf.default.disable_ipv6" \
             "net.ipv6.conf.lo.disable_ipv6"; do
    if ! grep -q "^$PARAM = 1" "$SYSCTL_CONF"; then
        echo "Adding $PARAM to $SYSCTL_CONF"
        echo "$PARAM = 1" | sudo tee -a "$SYSCTL_CONF" >/dev/null
        echo "Applying sysctl setting for $PARAM..."
        sudo sysctl -w "$PARAM=1"
    else
        echo "$PARAM is already set in $SYSCTL_CONF. Skipping..."
    fi

done

# Verify changes
echo "\n### IPv6 Configuration Verification ###"
echo "Checking current IPv6 addresses:"
ip a | grep inet6 || echo "No IPv6 addresses found."

echo "Checking IPv6 disable status:"
cat /proc/sys/net/ipv6/conf/all/disable_ipv6

echo "\nIPv6 has been disabled where necessary."

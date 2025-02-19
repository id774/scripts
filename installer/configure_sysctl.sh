#!/bin/sh

########################################################################
# configure_sysctl.sh: Configure IPv6 and Network Security Settings on GNU/Linux
#
#  Description:
#  This script configures IPv6 and network security settings on GNU/Linux
#  by modifying /etc/sysctl.conf. If the required settings are already present,
#  they will not be duplicated. After modification, it applies the changes and
#  verifies the configuration.
#
#  Features:
#  - Configures IPv6 settings by modifying sysctl parameters.
#  - Applies recommended security settings to prevent network attacks.
#  - Ensures changes are applied using sysctl only if needed.
#  - Verifies the applied configuration.
#  - Checks for necessary commands before execution.
#  - Ensures /etc/sysctl.conf exists before modification.
#  - Dynamically detects network interfaces to avoid setting non-existent ones.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-02-20
#       Added security settings for IPv4 (syncookies, ICMP redirects, source routing protection).
#  v1.0 2025-02-19
#       Initial release with IPv6 disabling functionality.
#
#  Usage:
#  ./configure_sysctl.sh --apply
#  --apply: Configures IPv6 and applies security settings by modifying /etc/sysctl.conf.
#
########################################################################

# Check if running on GNU/Linux
if [ "$(uname -s)" != "Linux" ]; then
    echo "Error: This script is only for GNU/Linux." >&2
    exit 1
fi

# Check for required argument
if [ "$1" != "--apply" ]; then
    echo "Usage: $0 --apply" >&2
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
check_commands sudo sysctl ip grep cat awk

# Define sysctl parameters
SYSCTL_CONF="/etc/sysctl.conf"
STATIC_PARAMS="net.ipv6.conf.all.disable_ipv6 = 1
net.ipv6.conf.default.disable_ipv6 = 1
net.ipv6.conf.lo.disable_ipv6 = 1
net.ipv4.tcp_syncookies = 1
net.ipv4.icmp_echo_ignore_broadcasts = 1
net.ipv4.conf.all.accept_redirects = 0
net.ipv4.conf.default.accept_redirects = 0
net.ipv4.conf.lo.accept_redirects = 0
net.ipv4.conf.all.accept_source_route = 0
net.ipv4.conf.default.accept_source_route = 0
net.ipv4.conf.lo.accept_source_route = 0"

# Get available network interfaces
INTERFACES=$(ip -o link show | awk -F': ' '{print $2}')

# Ensure /etc/sysctl.conf exists
if [ ! -f "$SYSCTL_CONF" ]; then
    echo "Error: $SYSCTL_CONF does not exist. Please create it and try again." >&2
    exit 1
fi

# Apply static parameters
echo "$STATIC_PARAMS" | while IFS='=' read -r KEY VALUE; do
    KEY=$(echo "$KEY" | tr -d ' ')
    VALUE=$(echo "$VALUE" | tr -d ' ')

    if [ -z "$KEY" ] || [ -z "$VALUE" ]; then
        continue
    fi

    if ! grep -q "^$KEY = $VALUE" "$SYSCTL_CONF"; then
        echo "Adding $KEY to $SYSCTL_CONF"
        echo "$KEY = $VALUE" | sudo tee -a "$SYSCTL_CONF" >/dev/null
        echo "Applying sysctl setting for $KEY..."
        sudo sysctl -w "$KEY=$VALUE"
    else
        echo "$KEY is already set in $SYSCTL_CONF. Skipping..."
    fi

done

# Apply interface-specific parameters
for iface in $INTERFACES; do
    for param in "accept_redirects" "accept_source_route"; do
        KEY="net.ipv4.conf.$iface.$param"
        VALUE="0"

        if [ -d "/proc/sys/net/ipv4/conf/$iface" ]; then
            if ! grep -q "^$KEY = $VALUE" "$SYSCTL_CONF"; then
                echo "Adding $KEY to $SYSCTL_CONF"
                echo "$KEY = $VALUE" | sudo tee -a "$SYSCTL_CONF" >/dev/null
                echo "Applying sysctl setting for $KEY..."
                sudo sysctl -w "$KEY=$VALUE"
            else
                echo "$KEY is already set in $SYSCTL_CONF. Skipping..."
            fi
        fi
    done

done

# Verify changes
echo "\n### IPv6 & Security Configuration Verification ###"
echo "Checking current IPv6 addresses:"
ip a | grep inet6 || echo "No IPv6 addresses found."

echo "Checking IPv6 disable status:"
cat /proc/sys/net/ipv6/conf/all/disable_ipv6

echo "\nIPv6 and security settings have been applied where necessary."

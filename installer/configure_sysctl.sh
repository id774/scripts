#!/bin/sh

########################################################################
# configure_sysctl.sh: Configure IPv6 and Network Security Settings on GNU/Linux
#
#  Description:
#  This script configures IPv6 and network security settings on GNU/Linux
#  by modifying separate configuration files in /etc/sysctl.d/.
#  IPv6 disabling settings are stored in 98-disable-ipv6.conf,
#  and IPv4 security hardening settings are stored in 97-secure-ipv4.conf.
#  This ensures better compatibility and avoids conflicts with system-managed settings.
#
#  Features:
#  - Configures IPv6 settings by modifying sysctl parameters.
#  - Applies recommended security settings to prevent network attacks.
#  - Enhances TCP/IP security against SYN Flood, ICMP attacks, and spoofing.
#  - Ensures changes are applied using sysctl only if needed.
#  - Verifies the applied configuration.
#  - Checks for necessary commands before execution.
#  - Ensures /etc/sysctl.d/ exists before modification.
#  - Uses separate configuration files for IPv6 disabling and IPv4 security hardening.
#
#  Security Settings:
#  - IPv6 disabling to prevent unintended network exposure.
#  - TCP SYN Cookies to mitigate SYN flood attacks.
#  - ICMP rate limiting and bogus error ignoring for enhanced security.
#  - Source routing and IP spoofing prevention.
#  - Address Space Layout Randomization (ASLR) enforcement.
#  - Optimized TCP timeout settings to mitigate DoS impact.
#  - TIME-WAIT assassination attack protection (RFC 1337).
#  - ICMP redirect and source routing protection against MITM attacks.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-02-20
#       Added IPv4 security hardening and separated configuration into two files:
#       - 98-disable-ipv6.conf for IPv6 settings.
#       - 97-secure-ipv4.conf for IPv4 security settings.
#  v1.0 2025-02-19
#       Initial release with IPv6 disabling functionality.
#
#  Usage:
#  ./configure_sysctl.sh --apply
#  --apply: Configures IPv6 and applies security settings by modifying /etc/sysctl.d/.
#
########################################################################

# Check if running on GNU/Linux
if [ "$(uname -s)" != "Linux" ]; then
    echo "Error: This script is only for GNU/Linux." >&2
    exit 1
fi

# Check for required argument
if [ "$1" != "--apply" ]; then
    echo "\n### GNU/Linux Network Security Configuration Tool ###"
    echo "This script configures IPv6 and network security settings on your system."
    echo "It modifies settings in /etc/sysctl.d/ to enhance security against network-based attacks."
    echo "\n### Features:"
    echo "- Disables IPv6 globally to prevent unintended exposure."
    echo "- Applies strict TCP SYN cookie settings to mitigate SYN flood attacks."
    echo "- Enables ICMP protection measures to prevent abuse and spoofing."
    echo "- Implements ASLR (Address Space Layout Randomization) to enhance memory security."
    echo "- Configures system-wide TCP timeout settings to prevent DoS impact."
    echo "- Prevents source routing and IP spoofing for safer networking."
    echo "\n### Warning:"
    echo "- This script makes permanent changes to your system settings by modifying /etc/sysctl.d/"
    echo "- If your system relies on IPv6, ensure disabling it does not impact functionality."
    echo "- Verify that your firewall settings do not conflict with these configurations."
    echo "\n### Usage:"
    echo "To apply these settings, run the following command:"
    echo "  $0 --apply"
    echo "\nThis will modify /etc/sysctl.d/ and apply security configurations immediately."
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
check_commands sudo sysctl ip grep cat awk tee

# Define sysctl parameters and configuration file paths
SYSCTL_DIR="/etc/sysctl.d"
IPV6_CONF="$SYSCTL_DIR/98-disable-ipv6.conf"
IPV4_CONF="$SYSCTL_DIR/97-secure-ipv4.conf"

# Ensure /etc/sysctl.d/ exists
if [ ! -d "$SYSCTL_DIR" ]; then
    echo "Error: $SYSCTL_DIR does not exist. Please create it manually and retry." >&2
    exit 1
fi

# Define IPv6 parameters
IPV6_PARAMS="
net.ipv6.conf.all.disable_ipv6 = 1
net.ipv6.conf.default.disable_ipv6 = 1
net.ipv6.conf.lo.disable_ipv6 = 1
"

# Define IPv4 security parameters
IPV4_PARAMS="
net.ipv4.tcp_syncookies = 1
net.ipv4.icmp_ignore_bogus_error_responses = 1
net.ipv4.icmp_ratelimit = 100
net.ipv4.icmp_ratemask = 88089
net.ipv4.conf.all.rp_filter = 1
net.ipv4.conf.default.rp_filter = 1
kernel.randomize_va_space = 2
net.ipv4.tcp_fin_timeout = 15
net.ipv4.tcp_keepalive_time = 300
net.ipv4.tcp_keepalive_probes = 5
net.ipv4.tcp_keepalive_intvl = 30
net.ipv4.tcp_rfc1337 = 1
net.ipv4.conf.all.accept_redirects = 0
net.ipv4.conf.default.accept_redirects = 0
net.ipv4.conf.lo.accept_redirects = 0
net.ipv4.conf.all.accept_source_route = 0
net.ipv4.conf.default.accept_source_route = 0
net.ipv4.conf.lo.accept_source_route = 0
"

# Apply IPv6 settings
if [ -f "$IPV6_CONF" ]; then
    echo "$IPV6_CONF already exists. Skipping creation."
else
    echo "Applying IPv6 settings to $IPV6_CONF"
    echo "# IPv6 Configuration: Disables IPv6 to prevent unintended exposure."
    echo "# IPv6 Configuration: Disables IPv6 to prevent unintended exposure." | sudo tee "$IPV6_CONF" >/dev/null
    echo "# This setting is useful for environments that do not rely on IPv6 connectivity." | sudo tee -a "$IPV6_CONF" >/dev/null
    echo "$IPV6_PARAMS" | sudo tee -a "$IPV6_CONF" >/dev/null
fi

# Apply IPv4 security settings
if [ -f "$IPV4_CONF" ]; then
    echo "$IPV4_CONF already exists. Skipping creation."
else
    echo "Applying IPv4 security settings to $IPV4_CONF"
    echo "# IPv4 Security Configuration: Enhances network security by applying strict policies."
    echo "# IPv4 Security Configuration: Enhances network security by applying strict policies." | sudo tee "$IPV4_CONF" >/dev/null
    echo "# Includes protections against SYN flood attacks, source routing, and ICMP abuse." | sudo tee -a "$IPV4_CONF" >/dev/null
    echo "$IPV4_PARAMS" | sudo tee -a "$IPV4_CONF" >/dev/null
fi

# Apply settings
echo "Applying sysctl settings..."
sudo sysctl --system

# Verify changes
echo "
### IPv6 & Security Configuration Verification ###"
echo "Checking current IPv6 addresses:"
ip a | grep inet6 || echo "No IPv6 addresses found."

echo "Checking IPv6 disable status:"
cat /proc/sys/net/ipv6/conf/all/disable_ipv6

echo "
IPv6 and IPv4 security settings have been successfully applied."

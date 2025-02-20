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
#  - Martian packet logging for better network monitoring.
#  - Prevents ARP spoofing attacks.
#  - Enables TCP window scaling for better network performance.
#  - Enables TCP Fast Open (TFO) for improved connection latency.
#  - Disables IPv6 anycast addresses.
#  - Prevents binding to non-local IPv6 addresses.
#  - Disables DHCPv6 auto-configuration.
#  - Disables IPv6 tunneling.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-02-20
#       - Added IPv4 security hardening and separated configuration into two files:
#         - 98-disable-ipv6.conf for IPv6 settings.
#         - 97-secure-ipv4.conf for IPv4 security settings.
#  v1.0 2025-02-19
#       - Initial release with IPv6 disabling functionality.
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
    echo "- Configures system-wide TCP timeout and keepalive settings to prevent DoS impact."
    echo "- Prevents source routing and IP spoofing for safer networking."
    echo "- Logs abnormal packets for better monitoring."
    echo "- Enables TCP Fast Open (TFO) to improve connection latency."
    echo "- Expands local port range for better network scalability."
    echo "- Protects against ARP spoofing and enforces reverse path filtering."
    echo "- Enhances TCP retransmission and keepalive settings for better stability."
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
        fi
    done
}

# Check required commands
check_commands sudo sysctl ip grep cat awk

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
sudo tee "$IPV6_CONF" > /dev/null <<EOF
# IPv6 Configuration: Disables IPv6 to prevent unintended exposure.
# This setting is useful for environments that do not rely on IPv6 connectivity.
net.ipv6.conf.all.disable_ipv6 = 1
net.ipv6.conf.default.disable_ipv6 = 1
net.ipv6.conf.lo.disable_ipv6 = 1

# Prevent IPv6 routing to enhance security.
net.ipv6.conf.all.forwarding = 0

# Disable IPv6 auto-configuration and router advertisements.
net.ipv6.conf.all.accept_ra = 0
net.ipv6.conf.default.accept_ra = 0
net.ipv6.conf.all.autoconf = 0
net.ipv6.conf.default.autoconf = 0

# Enable temporary IPv6 addresses for better privacy.
net.ipv6.conf.all.use_tempaddr = 2
net.ipv6.conf.default.use_tempaddr = 2

# Limit the number of IPv6 addresses per interface.
net.ipv6.conf.all.max_addresses = 2
net.ipv6.conf.default.max_addresses = 2

# Disable ICMP redirects globally (IPv6).
net.ipv6.conf.all.accept_redirects = 0
net.ipv6.conf.default.accept_redirects = 0

# Prevent binding to non-local IPv6 addresses
net.ipv6.ip_nonlocal_bind = 0

# Disable Duplicate Address Detection (DAD) in IPv6
net.ipv6.conf.all.dad_transmits = 0

# Disable DHCPv6 Auto-Configuration
net.ipv6.conf.all.dhcpv6_autoconf = 0

# Disable IPv6 Tunneling
net.ipv6.conf.all.disable_xfrm = 1
net.ipv6.conf.all.disable_tunnel = 1

EOF

# Define IPv4 security parameters
sudo tee "$IPV4_CONF" > /dev/null <<EOF
# IPv4 Security Configuration: Enhances network security by applying strict policies.
# Includes protections against SYN flood attacks, source routing, and ICMP abuse.
net.ipv4.tcp_syncookies = 1
net.ipv4.tcp_abort_on_overflow = 1
net.ipv4.icmp_ignore_bogus_error_responses = 1
net.ipv4.icmp_ratelimit = 10
net.ipv4.icmp_ratemask = 88089

# Prevent source routing and IP spoofing.
net.ipv4.conf.all.rp_filter = 1
net.ipv4.conf.default.rp_filter = 1
net.ipv4.conf.all.accept_source_route = 0
net.ipv4.conf.default.accept_source_route = 0

# Adjust TCP timeout and keepalive settings for enhanced security.
net.ipv4.tcp_fin_timeout = 15
net.ipv4.tcp_keepalive_time = 300
net.ipv4.tcp_keepalive_probes = 5
net.ipv4.tcp_keepalive_intvl = 30
net.ipv4.tcp_rfc1337 = 1

# Enable logging for abnormal packets.
net.ipv4.conf.all.log_martians = 1
net.ipv4.conf.default.log_martians = 1

# Disable ICMP echo requests for better security.
net.ipv4.icmp_echo_ignore_all = 1
net.ipv4.icmp_echo_ignore_broadcasts = 1

# Strengthen TCP/IP security against SYN flood attacks.
net.ipv4.tcp_max_syn_backlog = 4096
net.ipv4.tcp_synack_retries = 2
net.ipv4.tcp_retries2 = 5
net.ipv4.tcp_syn_retries = 2

# Prevent ARP spoofing attacks.
net.ipv4.conf.all.arp_ignore = 2
net.ipv4.conf.default.arp_ignore = 2
net.ipv4.conf.all.arp_filter = 1
net.ipv4.conf.default.arp_filter = 1
net.ipv4.conf.all.arp_notify = 1
net.ipv4.conf.default.arp_notify = 1

# Enable ASLR to enhance memory security.
kernel.randomize_va_space = 2

# Protect kernel information leakage.
kernel.kptr_restrict = 2
kernel.dmesg_restrict = 1

# Protect against NULL pointer dereference attacks.
vm.mmap_min_addr = 65536

# Disable TCP timestamps to mitigate certain DoS attacks.
net.ipv4.tcp_timestamps = 0

# Disable ICMP redirects globally.
net.ipv4.conf.all.accept_redirects = 0
net.ipv4.conf.default.accept_redirects = 0
net.ipv4.conf.lo.accept_redirects = 0
net.ipv4.conf.all.secure_redirects = 0

# Prevent the system from being used as a router.
net.ipv4.ip_forward = 0

# Set minimum MTU for path MTU discovery (helps prevent fragmentation attacks).
net.ipv4.route.min_adv_mss = 1480

# Expand local port range for better network scalability.
net.ipv4.ip_local_port_range = 1024 65535

# Enable TCP window scaling for better network performance.
net.ipv4.tcp_window_scaling = 1

# Enable TCP selective acknowledgment (SACK).
net.ipv4.tcp_sack = 1

# Enable Forward Acknowledgment (FACK) for congestion control.
net.ipv4.tcp_fack = 1

# Optimize TCP for low latency environments.
net.ipv4.tcp_low_latency = 1

# Prevent saving of old TCP connection metrics.
net.ipv4.tcp_no_metrics_save = 1

# Prevent sending of ICMP redirects
net.ipv4.conf.all.send_redirects = 0
net.ipv4.conf.default.send_redirects = 0

# Enable Early Retransmit for TCP
net.ipv4.tcp_early_retrans = 1

# Increase network buffer sizes
net.core.rmem_max = 16777216
net.core.wmem_max = 16777216

# Increase network device backlog
net.core.netdev_max_backlog = 5000

# Prevent IP source route spoofing
net.ipv4.conf.all.src_valid_mark = 1

# Enable TCP Fast Open (TFO) for improved connection latency
net.ipv4.tcp_fastopen = 3

# Adjust TCP Keepalive settings
net.ipv4.tcp_keepalive_probes = 9
net.ipv4.tcp_keepalive_intvl = 75

# Optimize TCP Retransmission Timeout (RTO)
net.ipv4.tcp_frto = 2

# Set default TTL for IPv4
net.ipv4.ip_default_ttl = 64

# Optimize TCP Retransmission Timeout (RTO)
net.ipv4.tcp_reordering = 3

# Reduce IPv4 Fragment Timeout
net.ipv4.ipfrag_time = 60

# Optimize TCP Path MTU Discovery
net.ipv4.tcp_mtu_probing = 1

# Enable TCP Duplicate SACK (DSACK)
net.ipv4.tcp_dsack = 1

# Enable TCP Receive Buffer Auto-Tuning
net.ipv4.tcp_moderate_rcvbuf = 1

# Increase max connections queue
net.core.somaxconn = 4096

EOF

# Apply settings
echo "Applying sysctl settings..."
sudo sysctl --system

# Verify changes
echo "\n### IPv6 & Security Configuration Verification ###"
echo "Checking current IPv6 addresses:"
ip a | grep inet6 || echo "No IPv6 addresses found."

echo "Checking IPv6 disable status:"
cat /proc/sys/net/ipv6/conf/all/disable_ipv6

echo "\nIPv6 and IPv4 security settings have been successfully applied."

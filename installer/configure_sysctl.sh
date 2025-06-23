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
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.9 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.8 2025-04-28
#       Added error detection and exit handling to write_config_file()
#       for safer configuration file creation.
#  v1.7 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.6 2025-03-25
#       Modified --apply to create config only if it doesn't exist.
#       Added --force-apply to always overwrite config files.
#  v1.5 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.4 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.3 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.2 2025-02-26
#       Enabled ICMP echo requests (ping) while enforcing rate limits.
#       Adjusted ICMP rate limiting for improved security.
#  v1.1 2025-02-20
#       Added IPv4 security hardening and separated configuration into two files:
#         - 98-disable-ipv6.conf for IPv6 settings.
#         - 97-secure-ipv4.conf for IPv4 security settings.
#  v1.0 2025-02-19
#       Initial release with IPv6 disabling functionality.
#
#  Usage:
#      ./configure_sysctl.sh --apply
#      ./configure_sysctl.sh --force-apply
#
#  --apply:         Creates config files only if they do not exist.
#  --force-apply:   Always overwrites and recreates config files.
#
#  Features:
#  - Checks for necessary commands before execution.
#  - Ensures /etc/sysctl.d/ exists before modification.
#  - Uses separate configuration files for IPv6 disabling and IPv4 security hardening.
#  - Configures IPv6 settings by modifying sysctl parameters.
#  - Applies recommended security settings to prevent network attacks.
#  - Enhances TCP/IP security against SYN Flood, ICMP attacks, and spoofing.
#  - Ensures changes are applied using sysctl only if needed.
#  - Verifies the applied configuration.
#
#  Security Settings:
#  - IPv6 disabling to prevent unintended network exposure.
#  - Disables IPv6 anycast addresses.
#  - Prevents binding to non-local IPv6 addresses.
#  - Disables DHCPv6 auto-configuration.
#  - Disables IPv6 tunneling.
#  - Source routing and IP spoofing prevention.
#  - ICMP redirect and source routing protection against MITM attacks.
#  - Prevents ARP spoofing attacks.
#  - Martian packet logging for better network monitoring.
#  - TCP SYN Cookies to mitigate SYN flood attacks.
#  - ICMP rate limiting and bogus error ignoring for enhanced security.
#  - Allows ICMP echo requests (ping) while enforcing rate limits to mitigate abuse.
#  - Optimized TCP timeout settings to mitigate DoS impact.
#  - TIME-WAIT assassination attack protection (RFC 1337).
#  - Enables TCP window scaling for better network performance.
#  - Enables TCP Fast Open (TFO) for improved connection latency.
#
#  Warning:
#  - If your system relies on IPv6, ensure disabling it does not impact functionality.
#  - Verify that your firewall settings do not conflict with these configurations.
#  - This script makes permanent changes to your system settings by modifying /etc/sysctl.d/
#  - This will modify /etc/sysctl.d/ and apply security configurations immediately.
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

# Check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Verify that required commands are available and executable
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

# Define IPv6 parameters
write_ipv6_config() {
    cat <<EOF
# IPv6 Configuration: Disables IPv6 to prevent unintended exposure.
# This setting is useful for environments that do not rely on IPv6 connectivity.

# Disable all IPv6 interfaces.
net.ipv6.conf.all.disable_ipv6 = 1
net.ipv6.conf.default.disable_ipv6 = 1
net.ipv6.conf.lo.disable_ipv6 = 1

# Prevent IPv6 routing to enhance security.
net.ipv6.conf.all.forwarding = 0

# Disable IPv6 auto-configuration and router advertisements.
net.ipv6.conf.all.autoconf = 0
net.ipv6.conf.default.autoconf = 0
net.ipv6.conf.all.accept_ra = 0
net.ipv6.conf.default.accept_ra = 0

# Enable temporary IPv6 addresses for better privacy.
net.ipv6.conf.all.use_tempaddr = 2
net.ipv6.conf.default.use_tempaddr = 2

# Limit the number of IPv6 addresses per interface.
net.ipv6.conf.all.max_addresses = 2
net.ipv6.conf.default.max_addresses = 2

# Disable ICMP redirects globally (IPv6).
net.ipv6.conf.all.accept_redirects = 0
net.ipv6.conf.default.accept_redirects = 0

# Prevent binding to non-local IPv6 addresses.
net.ipv6.ip_nonlocal_bind = 0

# Disable Duplicate Address Detection (DAD) in IPv6.
net.ipv6.conf.all.dad_transmits = 0

# Disable DHCPv6 Auto-Configuration.
net.ipv6.conf.all.dhcpv6_autoconf = 0

# Disable IPv6 Tunneling.
net.ipv6.conf.all.disable_xfrm = 1
net.ipv6.conf.all.disable_tunnel = 1

EOF
}

# Define IPv4 security parameters
write_ipv4_config() {
    cat <<EOF
# IPv4 Security Configuration: Enhances network security by applying strict policies.
# Includes protections against SYN flood attacks, source routing, and ICMP abuse.

# Protect against SYN flood attacks and optimize TCP behavior.
net.ipv4.tcp_syncookies = 1
net.ipv4.tcp_abort_on_overflow = 1
net.ipv4.tcp_max_syn_backlog = 4096
net.ipv4.tcp_synack_retries = 2
net.ipv4.tcp_retries2 = 5
net.ipv4.tcp_syn_retries = 2

# Improve ICMP security.
net.ipv4.icmp_ignore_bogus_error_responses = 1
net.ipv4.icmp_ratelimit = 100
net.ipv4.icmp_ratemask = 88089
net.ipv4.icmp_echo_ignore_all = 0
net.ipv4.icmp_echo_ignore_broadcasts = 1

# Prevent source routing and IP spoofing.
net.ipv4.conf.all.rp_filter = 1
net.ipv4.conf.default.rp_filter = 1
net.ipv4.conf.all.accept_source_route = 0
net.ipv4.conf.default.accept_source_route = 0

# Disable ICMP redirects globally.
net.ipv4.conf.all.accept_redirects = 0
net.ipv4.conf.default.accept_redirects = 0
net.ipv4.conf.lo.accept_redirects = 0
net.ipv4.conf.all.secure_redirects = 0
net.ipv4.conf.all.send_redirects = 0
net.ipv4.conf.default.send_redirects = 0

# Prevent ARP spoofing attacks.
net.ipv4.conf.all.arp_ignore = 2
net.ipv4.conf.default.arp_ignore = 2
net.ipv4.conf.all.arp_filter = 1
net.ipv4.conf.default.arp_filter = 1
net.ipv4.conf.all.arp_notify = 1
net.ipv4.conf.default.arp_notify = 1

# Adjust TCP timeout and keepalive settings.
net.ipv4.tcp_fin_timeout = 15
net.ipv4.tcp_keepalive_time = 300
net.ipv4.tcp_keepalive_probes = 9
net.ipv4.tcp_keepalive_intvl = 75
net.ipv4.tcp_rfc1337 = 1

# Optimize TCP retransmission and performance.
net.ipv4.tcp_frto = 2
net.ipv4.tcp_early_retrans = 1
net.ipv4.tcp_reordering = 3
net.ipv4.tcp_mtu_probing = 1
net.ipv4.tcp_fack = 1
net.ipv4.tcp_dsack = 1
net.ipv4.tcp_sack = 1
net.ipv4.tcp_fastopen = 3

# Enable TCP window scaling and receive buffer auto-tuning.
net.ipv4.tcp_window_scaling = 1
net.ipv4.tcp_moderate_rcvbuf = 1

# Reduce IPv4 Fragment Timeout.
net.ipv4.ipfrag_time = 60

# Set minimum MTU for path MTU discovery.
net.ipv4.route.min_adv_mss = 1480

# Prevent the system from being used as a router.
net.ipv4.ip_forward = 0

# Enable ASLR to enhance memory security.
kernel.randomize_va_space = 2

# Protect kernel information leakage.
kernel.kptr_restrict = 2
kernel.dmesg_restrict = 1

# Protect against NULL pointer dereference attacks.
vm.mmap_min_addr = 65536

# Disable TCP timestamps to mitigate certain DoS attacks.
net.ipv4.tcp_timestamps = 0

# Prevent saving of old TCP connection metrics.
net.ipv4.tcp_no_metrics_save = 1

# Set default TTL for IPv4.
net.ipv4.ip_default_ttl = 64

# Expand local port range for better network scalability.
net.ipv4.ip_local_port_range = 1024 65535

# Increase max connections queue.
net.core.somaxconn = 4096

# Increase network buffer sizes.
net.core.rmem_max = 16777216
net.core.wmem_max = 16777216

# Increase network device backlog.
net.core.netdev_max_backlog = 5000

# Prevent IP source route spoofing.
net.ipv4.conf.all.src_valid_mark = 1

EOF
}

# Write the specified sysctl config file.
# Behavior depends on the ACTION value (--apply or --force-apply).
write_config_file() {
    path="$1"
    writer="$2"

    case "$ACTION" in
        --force-apply)
            echo "[INFO] Writing $path (forced)..."
            if ! $writer | sudo tee "$path" > /dev/null; then
                echo "[ERROR] Failed to write $path." >&2
                exit 1
            fi
            return 1  # changed
            ;;
        --apply)
            if [ -e "$path" ]; then
                echo "[INFO] Skipping $path: already exists."
                return 0  # skipped
            else
                echo "[INFO] Writing $path..."
                if ! $writer | sudo tee "$path" > /dev/null; then
                    echo "[ERROR] Failed to write $path" >&2
                    exit 1
                fi
                return 1  # changed
            fi
            ;;
        *)
            echo "[ERROR] Internal Error: Unsupported ACTION '$ACTION'" >&2
            exit 2
            ;;
    esac
}

# Verify IPv6-related system state after applying sysctl configuration.
verify_sysctl_status() {
    # Verify IPv6 status
    echo ""
    echo "[INFO] ### IPv6 & Security Configuration Verification ###"

    echo "[INFO] Checking current IPv6 addresses:"
    ip a | grep inet6 || echo "[INFO] No IPv6 addresses found."

    # Display the current IPv6 address configuration.
    # This helps verify that IPv6 has been disabled successfully.
    echo "[INFO] Checking IPv6 disable status:"
    cat /proc/sys/net/ipv6/conf/all/disable_ipv6

    # Show current disable_ipv6 setting to confirm it is set to 1 (disabled).
    echo ""
    echo "[INFO] IPv6 and IPv4 security settings have been successfully applied."
}

# Main function to execute the script
main() {
    # Parse script argument (--apply or --force-apply) and validate input.
    ACTION="$1"
    case "$ACTION" in
        --apply|--force-apply)
            ;;
        *)
            usage
            ;;
    esac

    # Check prerequisites
    check_system
    check_commands sudo sysctl uname tee cat ip grep
    check_sudo

    # Define configuration paths
    SYSCTL_DIR="/etc/sysctl.d"
    IPV6_CONF="$SYSCTL_DIR/98-disable-ipv6.conf"
    IPV4_CONF="$SYSCTL_DIR/97-secure-ipv4.conf"

    # Ensure /etc/sysctl.d exists
    if [ ! -d "$SYSCTL_DIR" ]; then
        echo "[ERROR] $SYSCTL_DIR does not exist. Please create it manually and retry." >&2
        exit 1
    fi

    changed=0
    # Write config files as needed
    write_config_file "$IPV6_CONF" write_ipv6_config || changed=1
    write_config_file "$IPV4_CONF" write_ipv4_config || changed=1

    # Apply sysctl changes
    if [ "$changed" -eq 1 ]; then
        echo "[INFO] Applying sysctl settings..."
        sudo sysctl --system
        verify_sysctl_status
    else
        echo "[INFO] No configuration changes detected. Skipping sysctl reload."
    fi
    return 0
}

# Execute main function
main "$@"
exit $?

#!/bin/sh

########################################################################
# configure_sysctl.sh: Configure IPv6 and Network Security Settings on GNU/Linux
#
#  Description:
#  This script configures IPv6 and network security settings on GNU/Linux
#  by writing configuration snippets into /etc/sysctl.d/.
#  IPv6 disabling settings are stored in 98-disable-ipv6.conf,
#  and IPv4 security hardening settings are stored in 97-secure-ipv4.conf.
#
#  Notes for Debian 13 “trixie” and later:
#  - systemd-sysctl no longer reads /etc/sysctl.conf.
#    Place local settings under /etc/sysctl.d/*.conf instead.
#  - This script emits only sysctl keys that actually exist on the running kernel
#    (it probes /proc/sys). This avoids warnings on both older and newer kernels.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
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
#  - Requires /etc/sysctl.d/ to exist (does not create it).
#  - Uses separate configuration files for IPv6 disabling and IPv4 security hardening.
#  - Emits only existing sysctl keys (probes /proc/sys), for cross-kernel compatibility.
#  - Applies recommended security settings to prevent typical network attacks.
#  - Verifies a subset of applied configuration after reload.
#
#  Security Settings (high level):
#  - Optionally disable IPv6 (interface-wide) to avoid unintended exposure.
#  - Disable ICMP redirects, source routing; enable anti-spoof measures.
#  - Enable SYN cookies; tune TCP keepalive/FIN timeout; RFC1337 protection.
#  - Keep TCP timestamps enabled by default (availability-friendly).
#
#  Warning:
#  - If your system relies on IPv6, ensure disabling it does not impact functionality.
#  - Review rp_filter/ARP settings for asymmetric routing or multi-homing scenarios.
#  - This script makes permanent changes by writing /etc/sysctl.d/*.conf
#    and applies them immediately via `sysctl --system`.
#
#  Version History:
#  v1.10 2025-10-11
#        Emit only sysctl keys that exist on the running kernel (/proc/sys probe).
#        Remove/avoid legacy or conflicting keys (e.g., tcp_frto, tcp_fack,
#        IPv6 use_tempaddr/max_addresses when disabling IPv6, dhcpv6_autoconf).
#        Keep TCP timestamps enabled by default; expand verification summary.
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
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
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

# Map sysctl name to /proc path
sysctl_path() {
    # Convert net.ipv4.tcp_syncookies -> /proc/sys/net/ipv4/tcp_syncookies
    echo "/proc/sys/$(printf '%s\n' "$1" | tr . /)"
}

# Check if a sysctl key exists on this kernel
 has_sysctl() {
     p=$(sysctl_path "$1")
     [ -f "$p" ]
 }

# Print "key = value" only if the key exists on this kernel
emit_sysctl() {
    key="$1"
    val="$2"
    if has_sysctl "$key"; then
        printf '%s = %s\n' "$key" "$val"
    else
        :
    fi
}

# Define IPv6 parameters (filtered emission)
write_ipv6_config() {
    # Fully disable IPv6 (interface-wide)
    emit_sysctl net.ipv6.conf.all.disable_ipv6 1
    emit_sysctl net.ipv6.conf.default.disable_ipv6 1
    emit_sysctl net.ipv6.conf.lo.disable_ipv6 1

    # Prevent routing and autoconf/RA
    emit_sysctl net.ipv6.conf.all.forwarding 0
    emit_sysctl net.ipv6.conf.all.autoconf 0
    emit_sysctl net.ipv6.conf.default.autoconf 0
    emit_sysctl net.ipv6.conf.all.accept_ra 0
    emit_sysctl net.ipv6.conf.default.accept_ra 0
    emit_sysctl net.ipv6.conf.all.accept_redirects 0
    emit_sysctl net.ipv6.conf.default.accept_redirects 0
    emit_sysctl net.ipv6.ip_nonlocal_bind 0

    # Intentionally skip: use_tempaddr/max_addresses when disabling IPv6 entirely.
    # Also skip environment-specific keys (dad_transmits, dhcpv6_autoconf,
    # disable_xfrm, disable_tunnel) unless explicitly required.
}

# Define IPv4 security parameters (filtered emission)
write_ipv4_config() {
    # TCP hardening
    emit_sysctl net.ipv4.tcp_syncookies 1
    emit_sysctl net.ipv4.tcp_abort_on_overflow 1
    emit_sysctl net.ipv4.tcp_max_syn_backlog 4096
    emit_sysctl net.ipv4.tcp_synack_retries 2
    emit_sysctl net.ipv4.tcp_retries2 5
    emit_sysctl net.ipv4.tcp_syn_retries 2

    # ICMP behavior
    emit_sysctl net.ipv4.icmp_ignore_bogus_error_responses 1
    emit_sysctl net.ipv4.icmp_ratelimit 100
    emit_sysctl net.ipv4.icmp_ratemask 88089
    emit_sysctl net.ipv4.icmp_echo_ignore_all 0
    emit_sysctl net.ipv4.icmp_echo_ignore_broadcasts 1

    # Anti-spoof / redirects
    emit_sysctl net.ipv4.conf.all.rp_filter 1
    emit_sysctl net.ipv4.conf.default.rp_filter 1
    emit_sysctl net.ipv4.conf.all.accept_source_route 0
    emit_sysctl net.ipv4.conf.default.accept_source_route 0
    emit_sysctl net.ipv4.conf.all.accept_redirects 0
    emit_sysctl net.ipv4.conf.default.accept_redirects 0
    emit_sysctl net.ipv4.conf.lo.accept_redirects 0
    emit_sysctl net.ipv4.conf.all.secure_redirects 0
    emit_sysctl net.ipv4.conf.all.send_redirects 0
    emit_sysctl net.ipv4.conf.default.send_redirects 0

    # ARP settings (review for single-homed vs multi-homed)
    emit_sysctl net.ipv4.conf.all.arp_ignore 2
    emit_sysctl net.ipv4.conf.default.arp_ignore 2
    emit_sysctl net.ipv4.conf.all.arp_filter 1
    emit_sysctl net.ipv4.conf.default.arp_filter 1
    emit_sysctl net.ipv4.conf.all.arp_notify 1
    emit_sysctl net.ipv4.conf.default.arp_notify 1

    # Timeouts / keepalive
    emit_sysctl net.ipv4.tcp_fin_timeout 15
    emit_sysctl net.ipv4.tcp_keepalive_time 300
    emit_sysctl net.ipv4.tcp_keepalive_probes 9
    emit_sysctl net.ipv4.tcp_keepalive_intvl 75
    emit_sysctl net.ipv4.tcp_rfc1337 1

    # Performance (legacy keys auto-skip if absent)
    emit_sysctl net.ipv4.tcp_early_retrans 1
    emit_sysctl net.ipv4.tcp_reordering 3
    emit_sysctl net.ipv4.tcp_mtu_probing 1
    emit_sysctl net.ipv4.tcp_dsack 1
    emit_sysctl net.ipv4.tcp_sack 1
    emit_sysctl net.ipv4.tcp_fastopen 3
    emit_sysctl net.ipv4.tcp_window_scaling 1
    emit_sysctl net.ipv4.tcp_moderate_rcvbuf 1

    # Fragment / routing
    emit_sysctl net.ipv4.ipfrag_time 60
    emit_sysctl net.ipv4.route.min_adv_mss 1480
    emit_sysctl net.ipv4.ip_forward 0

    # Kernel & VM
    emit_sysctl kernel.randomize_va_space 2
    emit_sysctl kernel.kptr_restrict 2
    emit_sysctl kernel.dmesg_restrict 1
    emit_sysctl vm.mmap_min_addr 65536

    # Keep TCP timestamps enabled by default (availability-friendly)
    emit_sysctl net.ipv4.tcp_timestamps 1

    # Metrics / defaults
    emit_sysctl net.ipv4.tcp_no_metrics_save 1
    emit_sysctl net.ipv4.ip_default_ttl 64
    emit_sysctl net.ipv4.ip_local_port_range "1024 65535"
    emit_sysctl net.core.somaxconn 4096
    emit_sysctl net.core.rmem_max 16777216
    emit_sysctl net.core.wmem_max 16777216
    emit_sysctl net.core.netdev_max_backlog 5000
    emit_sysctl net.ipv4.conf.all.src_valid_mark 1
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

# Main entry point of the script
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
    check_commands sudo sysctl uname tee cat ip grep tr
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

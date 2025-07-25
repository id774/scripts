#!/bin/sh

########################################################################
# set_ipv6_macos.sh: Enable or Disable IPv6 on macOS
#
#  Description:
#  This script enables or disables IPv6 on all network interfaces detected on macOS.
#  It ensures idempotency by checking the current IPv6 setting before applying changes.
#  If the system is not macOS, the script exits with return code 1.
#
#  Features:
#  - Detects all active network interfaces dynamically.
#  - Enables or disables IPv6 based on the provided flag.
#  - Ensures compatibility with POSIX shell.
#  - Requires sudo for modifying network settings.
#  - Exits with return code 1 if executed on a non-macOS system.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./set_ipv6_macos.sh --enable | --disable
#
#  --enable: Enables IPv6 on all detected network services.
#  --disable: Disables IPv6 on all detected network services.
#
#  Version History:
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-02-19
#       Initial release with full macOS IPv6 disabling support.
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

# Check if the system is macOS
check_system() {
    if [ "$(uname)" != "Darwin" ]; then
        echo "[ERROR] This script is intended for macOS only." >&2
        exit 1
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Check for required argument
    if [ "$#" -ne 1 ]; then
        usage
    fi

    # Parse arguments
    if [ "$1" = "--enable" ]; then
        echo "[INFO] Enabling IPv6 on Wi-Fi and Ethernet..."
        for SERVICE in Wi-Fi Ethernet; do
            CURRENT_SETTING=$(networksetup -getinfo "$SERVICE" | awk -F': ' '/IPv6/{print $2}' | head -n 1)
            if [ "$CURRENT_SETTING" != "Automatic" ]; then
                echo "[INFO] Setting IPv6 to Automatic on $SERVICE..."
                sudo networksetup -setv6automatic "$SERVICE"
            else
                echo "[INFO] IPv6 is already enabled on $SERVICE. Skipping..."
            fi
        done

        # Verify IPv6 settings after modification
        echo "[INFO] Verification of IPv6 settings:"
        for SERVICE in Wi-Fi Ethernet; do
            echo "$SERVICE: $(networksetup -getinfo "$SERVICE" | awk -F': ' '/IPv6/{print $2}' | head -n 1)"
        done

        echo "[INFO] IPv6 has been enabled where necessary."
        return 0
    fi

    if [ "$1" = "--disable" ]; then
        echo "[INFO] Disabling IPv6 on all network services..."

        # Get all network services
        SERVICES=$(networksetup -listallnetworkservices | tail -n +2)

        for SERVICE in $SERVICES; do
            # Check if the service is valid
            if ! networksetup -getinfo "$SERVICE" >/dev/null 2>&1; then
                echo "[INFO] Skipping unrecognized network service: $SERVICE" >&2
                continue
            fi

            # Get the current IPv6 setting for the service
            CURRENT_SETTING=$(networksetup -getinfo "$SERVICE" | awk -F': ' '/IPv6/{print $2}' | head -n 1)

            if [ "$CURRENT_SETTING" != "Off" ]; then
                echo "[INFO] Disabling IPv6 on $SERVICE..."
                sudo networksetup -setv6off "$SERVICE" || echo "[ERROR] Failed to disable IPv6 on $SERVICE." >&2
            else
                echo "[INFO] IPv6 is already disabled on $SERVICE. Skipping..."
            fi
        done

        # Verify IPv6 settings after modification
        for SERVICE in $SERVICES; do
            echo "$SERVICE: $(networksetup -getinfo "$SERVICE" | awk -F': ' '/IPv6/{print $2}' | head -n 1)"
        done

        echo "[INFO] IPv6 has been modified where necessary."
        return 0
    fi

    # If an invalid argument is provided, show usage
    usage
}

# Execute main function
main "$@"
exit $?

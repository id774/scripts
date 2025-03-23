#!/bin/sh

########################################################################
# toggle_ipv6_macos.sh: Enable/Disable IPv6 on macOS
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
#  Version History:
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-02-19
#       Initial release with full macOS IPv6 disabling support.
#
#  Usage:
#      ./toggle_ipv6_macos.sh --enable | --disable
#
#  --enable: Enables IPv6 on all detected network services.
#  --disable: Disables IPv6 on all detected network services.
#
########################################################################

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

# Function to check if the system is macOS
check_system() {
    if [ "$(uname)" != "Darwin" ]; then
        echo "Error: This script is intended for macOS only." >&2
        exit 1
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    # Check for required argument
    if [ "$#" -ne 1 ]; then
        usage
    fi

    # Parse arguments
    if [ "$1" = "--enable" ]; then
        echo "Enabling IPv6 on Wi-Fi and Ethernet..."
        for SERVICE in Wi-Fi Ethernet; do
            CURRENT_SETTING=$(networksetup -getinfo "$SERVICE" | awk -F': ' '/IPv6/{print $2}' | head -n 1)
            if [ "$CURRENT_SETTING" != "Automatic" ]; then
                echo "Setting IPv6 to Automatic on $SERVICE..."
                sudo networksetup -setv6automatic "$SERVICE"
            else
                echo "IPv6 is already enabled on $SERVICE. Skipping..."
            fi
        done

        # Verify IPv6 settings after modification
        echo "Verification of IPv6 settings:"
        for SERVICE in Wi-Fi Ethernet; do
            echo "$SERVICE: $(networksetup -getinfo "$SERVICE" | awk -F': ' '/IPv6/{print $2}' | head -n 1)"
        done

        echo "IPv6 has been enabled where necessary."
        exit 0
    fi

    if [ "$1" = "--disable" ]; then
        echo "Disabling IPv6 on all network services..."

        # Get all network services
        SERVICES=$(networksetup -listallnetworkservices | tail -n +2)

        for SERVICE in $SERVICES; do
            # Check if the service is valid
            if ! networksetup -getinfo "$SERVICE" >/dev/null 2>&1; then
                echo "Skipping unrecognized network service: $SERVICE" >&2
                continue
            fi

            # Get the current IPv6 setting for the service
            CURRENT_SETTING=$(networksetup -getinfo "$SERVICE" | awk -F': ' '/IPv6/{print $2}' | head -n 1)

            if [ "$CURRENT_SETTING" != "Off" ]; then
                echo "Disabling IPv6 on $SERVICE..."
                sudo networksetup -setv6off "$SERVICE" || echo "Error: Failed to disable IPv6 on $SERVICE." >&2
            else
                echo "IPv6 is already disabled on $SERVICE. Skipping..."
            fi
        done

        # Verify IPv6 settings after modification
        for SERVICE in $SERVICES; do
            echo "$SERVICE: $(networksetup -getinfo "$SERVICE" | awk -F': ' '/IPv6/{print $2}' | head -n 1)"
        done

        echo "IPv6 has been modified where necessary."
        exit 0
    fi

    # If an invalid argument is provided, show usage
    usage
}

# Execute main function
main "$@"

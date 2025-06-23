#!/bin/sh

########################################################################
# disable_freshclam_syslog.sh: Suppress syslog output from freshclam
#
#  Description:
#  This script overrides the systemd service for clamav-freshclam to
#  redirect both stdout and stderr to /dev/null, effectively preventing
#  log messages from appearing in syslog. It creates a systemd drop-in
#  override file and restarts the affected service.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.0 2025-05-10
#       Initial release. Implements override drop-in to redirect
#       freshclam output away from syslog via systemd unit settings.
#
#  Usage:
#      ./disable_freshclam_syslog.sh
#      This script can be executed as a normal user. It uses sudo internally.
#
#  Notes:
#  - This script has no effect on journal output unless forwarded to syslog.
#  - Use `journalctl -u clamav-freshclam` to view remaining logs.
#
#  Error Conditions:
#  1. General failure.
#  2. Failed to create override directory.
#  3. Failed to write override file.
#  4. Failed to restart systemd services.
#  126. Required command is not executable.
#  127. Required command is not installed.
#
########################################################################

SERVICE_NAME="clamav-freshclam.service"
OVERRIDE_DIR="/etc/systemd/system/$SERVICE_NAME.d"
OVERRIDE_FILE="$OVERRIDE_DIR/override.conf"

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Function to check if the system is Linux
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

# Function to check required commands
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

# Create the systemd drop-in override file to suppress freshclam output
# by redirecting both stdout and stderr to /dev/null.
create_override() {
    if ! sudo mkdir -p "$OVERRIDE_DIR"; then
        echo "[ERROR] Failed to create directory: $OVERRIDE_DIR" >&2
        exit 2
    fi

    if ! printf '[Service]\nStandardOutput=null\nStandardError=null\n' \
        | sudo tee "$OVERRIDE_FILE" >/dev/null; then
        echo "[ERROR] Failed to write override file: $OVERRIDE_FILE" >&2
        exit 3
    fi
    echo "[INFO] Override file created at $OVERRIDE_FILE."
}

# Reload systemd manager configuration to apply the override settings.
reload_systemd() {
    echo "[INFO] Reloading systemd configuration..."
    sudo systemctl daemon-reexec
    sudo systemctl daemon-reload
}

# Restart the clamav-freshclam service to activate the new override configuration.
restart_service() {
    echo "[INFO] Restarting $SERVICE_NAME..."
    if ! sudo systemctl restart "$SERVICE_NAME"; then
        echo "[ERROR] Failed to restart $SERVICE_NAME." >&2
        exit 4
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands sudo mkdir tee systemctl
    check_sudo
    create_override
    reload_systemd
    restart_service
    echo "[INFO] Syslog output for $SERVICE_NAME has been suppressed."
    return 0
}

# Execute main function
main "$@"
exit $?

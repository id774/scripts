#!/bin/sh

########################################################################
# setup_karabiner.sh: Configure Karabiner settings on macOS
#
#  Description:
#  This script sets up Karabiner configuration by:
#  - Ensuring the Karabiner configuration directory exists.
#  - Copying a predefined configuration file to the appropriate location.
#  - Creating a backup of an existing configuration file if present.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./setup_karabiner.sh
#
#  Requirements:
#  - Must be executed on macOS.
#  - Requires `karabiner.json` in `$SCRIPTS/dot_files/dot_karabiner.d/configuration/`
#
#  Version History:
#  v1.4 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.3 2025-04-27
#       Add critical failure checks to Karabiner setup steps.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-16
#       Added environment validation, file existence check, and backup handling.
#  v0.1 2017-01-02
#       Initial version.
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

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing Karabiner configurations." >&2
        exit 1
    fi
}

# Copy Karabiner configuration
setup_karabiner() {
    CONFIG_DIR="$HOME/.karabiner.d/configuration"
    SRC_CONFIG="$SCRIPTS/dot_files/dot_karabiner.d/configuration/karabiner.json"
    DEST_CONFIG="$CONFIG_DIR/karabiner.json"

    echo "[INFO] Ensuring configuration directory exists."
    if ! mkdir -p "$CONFIG_DIR"; then
        echo "[ERROR] Failed to create configuration directory: $CONFIG_DIR." >&2
        exit 1
    fi

    if [ ! -f "$SRC_CONFIG" ]; then
        echo "[ERROR] Configuration file '$SRC_CONFIG' not found." >&2
        exit 1
    fi

    if [ -f "$DEST_CONFIG" ]; then
        BACKUP_FILE="$DEST_CONFIG.bak.$(date +%Y%m%d%H%M%S)"
        echo "[INFO] Backing up existing configuration to $BACKUP_FILE."
        if ! mv "$DEST_CONFIG" "$BACKUP_FILE"; then
            echo "[ERROR] Failed to backup existing configuration file." >&2
            exit 1
        fi
    fi

    echo "[INFO] Copying new configuration."
    if ! cp "$SRC_CONFIG" "$DEST_CONFIG"; then
        echo "[ERROR] Failed to copy new configuration file." >&2
        exit 1
    fi

    echo "[INFO] Karabiner configuration successfully updated."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_scripts
    setup_karabiner
    return 0
}

# Execute main function
main "$@"
exit $?

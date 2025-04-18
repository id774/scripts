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
#  Version History:
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-16
#       Added environment validation, file existence check, and backup handling.
#  v0.1 2017-01-02
#       Initial version.
#
#  Usage:
#      ./setup_karabiner.sh
#
#  Requirements:
#  - Must be executed on macOS.
#  - Requires `karabiner.json` in `$SCRIPTS/dot_files/dot_karabiner.d/configuration/`
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
        echo "[ERROR] This script is intended for macOS only." >&2
        exit 1
    fi
}

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing Karabiner configurations." >&2
        exit 1
    fi
}

# Function to copy Karabiner configuration
setup_karabiner() {
    CONFIG_DIR="$HOME/.karabiner.d/configuration"
    SRC_CONFIG="$SCRIPTS/dot_files/dot_karabiner.d/configuration/karabiner.json"
    DEST_CONFIG="$CONFIG_DIR/karabiner.json"

    # Ensure configuration directory exists
    mkdir -p "$CONFIG_DIR"

    # Check if the source configuration file exists
    if [ ! -f "$SRC_CONFIG" ]; then
        echo "[ERROR] Configuration file '$SRC_CONFIG' not found." >&2
        exit 1
    fi

    # Backup existing configuration if present
    if [ -f "$DEST_CONFIG" ]; then
        BACKUP_FILE="$DEST_CONFIG.bak.$(date +%Y%m%d%H%M%S)"
        echo "[INFO] Backing up existing configuration to $BACKUP_FILE"
        mv "$DEST_CONFIG" "$BACKUP_FILE"
    fi

    # Copy new configuration
    cp "$SRC_CONFIG" "$DEST_CONFIG"
    echo "[INFO] Karabiner configuration successfully updated."
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_scripts
    setup_karabiner
}

# Execute main function
main "$@"

#!/bin/sh

########################################################################
# setup_gnome_desktop_gsettings.sh: Configure GNOME media and UI settings
#
#  Description:
#  Configure GNOME Flashback friendly settings:
#    - Disable automount and autorun for external media
#    - Hide desktop icons
#    - Set fixed number of workspaces
#    - Import keyboard shortcuts and WM keybindings via dconf
#    - Install xfce4-terminal profile used in Flashback session
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./setup_gnome_desktop_gsettings.sh
#
#  Features:
#  - Validate Linux environment and required commands
#  - Require user session DBus for gsettings/dconf
#  - Log and verify each setting or import step
#
#  Version History:
#  v1.4 2025-08-11
#       Refactor into functions, add dconf logging and verification,
#       add session bus check, enforce quoting and error handling.
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-02-26
#       Initial release with command validation and setting updates.
#
########################################################################

# Print header block as usage text
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check Linux OS
check_system() {
    os="$(uname -s 2>/dev/null)"
    if [ "$os" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Ensure SCRIPTS env var is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set SCRIPTS to the root of your script collection." >&2
        exit 1
    fi
}

# Ensure we have a user session DBus (required for gsettings/dconf)
check_session_bus() {
    if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
        echo "[ERROR] DBUS session bus is not available." >&2
        echo "Run this script inside a logged-in desktop session." >&2
        exit 1
    fi
}

# Check required commands exist and are executable
check_commands() {
    for cmd in "$@"; do
        path="$(command -v "$cmd" 2>/dev/null)"
        if [ -z "$path" ]; then
            echo "[ERROR] Command not found: $cmd" >&2
            exit 127
        fi
        if [ ! -x "$path" ]; then
            echo "[ERROR] Command not executable: $cmd" >&2
            exit 126
        fi
    done
}

# Set a gsettings key and confirm value
gsettings_settings() {
    # Args: SCHEMA KEY VALUE
    schema="$1"
    key="$2"
    value="$3"

    echo "[INFO] Setting: $schema $key -> $value"
    if ! gsettings set "$schema" "$key" "$value"; then
        echo "[ERROR] Failed to set $schema $key to $value" >&2
        exit 1
    fi

    echo -n "[INFO] Confirming: $schema $key = "
    if ! gsettings get "$schema" "$key"; then
        echo "[ERROR] Failed to read back $schema $key" >&2
        exit 1
    fi
}

# Apply media handling settings
apply_media_handling_settings() {
    gsettings_settings org.gnome.desktop.media-handling automount false
    gsettings_settings org.gnome.desktop.media-handling automount-open false
    gsettings_settings org.gnome.desktop.media-handling autorun-never true
}

# Apply UI settings (desktop icons, workspaces)
apply_ui_settings() {
    gsettings_settings org.gnome.desktop.background show-desktop-icons false
    gsettings_settings org.gnome.desktop.wm.preferences num-workspaces 9
}

# Load dconf subtree from file and verify
dconf_load_settings() {
    # Args: DC_PATH FILE
    dc_path="$1"
    dc_file="$2"

    if [ ! -r "$dc_file" ]; then
        echo "[ERROR] dconf source not found: $dc_file" >&2
        exit 1
    fi

    echo "[INFO] Loading dconf path $dc_path from $dc_file"
    if ! dconf load "$dc_path" < "$dc_file"; then
        echo "[ERROR] dconf load failed for $dc_path" >&2
        exit 1
    fi

    after_dump="$(dconf dump "$dc_path" 2>/dev/null)"
    if [ -z "$after_dump" ]; then
        echo "[ERROR] dconf dump is empty after load for $dc_path" >&2
        exit 1
    fi

    # Compare number of key=value lines present after load
    expected="$(awk -F= '/^[a-zA-Z0-9_-]+=/{print $1"="$2}' "$dc_file" | wc -l | tr -d ' ')"
    applied="$(awk -F= '/^[a-zA-Z0-9_-]+=/{print $1"="$2}' "$dc_file" \
        | while IFS= read -r kv; do echo "$after_dump" | grep -Fqx "$kv" && echo ok; done \
        | wc -l | tr -d ' ')"

    echo "[INFO] Confirming dconf $dc_path keys applied: expected=$expected applied=$applied"
    if [ "$expected" != "$applied" ]; then
        echo "[ERROR] dconf confirmation mismatch under $dc_path" >&2
        exit 1
    fi
}

# Import GNOME keybindings (media keys and WM bindings)
import_gnome_keybindings() {
    dconf_load_settings "/org/gnome/settings-daemon/plugins/media-keys/" "$SCRIPTS/etc/gnome/gnome-shortcuts.conf"
    dconf_load_settings "/org/gnome/desktop/wm/keybindings/"         "$SCRIPTS/etc/gnome/gnome-wm-keys.conf"
}

# Install xfce4-terminal profile for Flashback session
install_xfce4_terminal_profile() {
    dst="$HOME/.config/xfce4/terminal"
    src="$SCRIPTS/etc/xfce/terminalrc"

    if [ ! -r "$src" ]; then
        echo "[ERROR] terminalrc not found: $src" >&2
        exit 1
    fi

    echo "[INFO] Installing xfce4-terminal profile to $dst"
    if ! mkdir -p "$dst"; then
        echo "[ERROR] Failed to create directory: $dst" >&2
        exit 1
    fi
    if ! cp "$src" "$dst/"; then
        echo "[ERROR] Failed to copy terminalrc to $dst" >&2
        exit 1
    fi
    echo "[INFO] xfce4-terminal profile installed"
}

# Main
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_scripts
    check_session_bus
    check_commands gsettings dconf mkdir cp

    apply_media_handling_settings
    apply_ui_settings
    install_xfce4_terminal_profile
    import_gnome_keybindings

    echo "[INFO] GNOME settings have been updated successfully."
    return 0
}

main "$@"
exit $?

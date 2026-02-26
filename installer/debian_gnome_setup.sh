#!/bin/sh

########################################################################
# debian_gnome_setup.sh: Apply GNOME settings for GNOME Shell session
#
#  Description:
#  Apply GNOME settings used with GNOME Shell. This script:
#    - Media handling: disable automount/automount-open and set autorun-never=true
#    - Workspaces: disable dynamic workspaces and set a fixed count (default 9)
#    - WM keybindings (via dconf load, gnome-wm-keys.conf):
#        * Switch to workspace 1..9 with <Primary>1..9
#        * Move window to workspace 1..9 with <Primary><Alt>1..9
#        * Switch/move by direction with <Control><Alt>Arrow and <Shift><Control><Alt>Arrow
#        * Disable/adjust other window actions as defined (close/minimize/switch-windows etc.)
#    - Media keys and custom app shortcuts (via dconf load, gnome-shortcuts.conf)
#    - Lock & idle behavior: keep manual lock, disable auto lock; set idle-delay=0 and lock-delay=0
#    - Appearance: prefer dark color scheme
#    - Keyboard repeat: enable repeat; default delay=200ms and interval=25ms (overridable via env)
#    - Profiles: install xfce4-terminal profile if xfce4-terminal is installed
#    - Disable services: mask background services for a server-like desktop (Tracker/GOA/Evolution/Rygel)
#    - Confirm before applying settings
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./debian_gnome_setup.sh
#
#  Environment:
#      SCRIPTS: path to the root of this repo (required)
#      GNOME_WORKSPACES: fixed workspace count (default: 9)
#      KEY_REPEAT_DELAY_MS: repeat delay ms (default: 200)
#      KEY_REPEAT_INTERVAL_MS: repeat interval ms (default: 25)
#      DISABLE_SERVICES: "yes" to apply service mask steps (default: yes)
#      If xfce4-terminal is installed, this script installs its terminal profile.
#
#  Error Conditions:
#  - If required commands are missing, execution is halted.
#  - If DBus session is not available, execution is halted.
#
#  Version History:
#  v1.1 2026-02-26
#       Prompt for confirmation before applying settings.
#  v1.0 2026-02-14
#       Initial GNOME Shell version based on debian_gnome_flashback_setup.sh.
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
    os="$(uname -s 2>/dev/null)"
    if [ "$os" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if the SCRIPTS variable is unset or empty
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set SCRIPTS to the root of your script collection." >&2
        exit 1
    fi
}

# Check if required commands are available and executable
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

# Ensure we have a user session DBus (required for gsettings/dconf)
check_session_bus() {
    if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
        echo "[ERROR] DBUS session bus is not available." >&2
        echo "Run this script inside a logged-in desktop session." >&2
        exit 1
    fi
}

# ----- gsettings helpers -------------------------------------------------

# Check if a gsettings key is writable (exists)
gsettings_can_set() {
    # Args: SCHEMA KEY
    schema="$1"
    key="$2"
    gsettings writable "$schema" "$key" >/dev/null 2>&1
}

# Set a gsettings key and confirm value
gsettings_settings() {
    # Args: SCHEMA KEY VALUE
    schema="$1"
    key="$2"
    value="$3"

    # Skip keys that are not present or not writable on this GNOME version
    if ! gsettings_can_set "$schema" "$key"; then
        echo "[WARN] Skipping unknown or read-only key: $schema $key" >&2
        return 0
    fi

    echo "[INFO] Setting: $schema $key -> $value"
    if ! gsettings set "$schema" "$key" "$value"; then
        echo "[ERROR] Failed to set $schema $key to $value" >&2
        exit 1
    fi

    # Read back the value for verification
    printf "%s" "[INFO] Confirming: $schema $key = "
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

# Apply workspace settings for GNOME Shell (fixed workspaces)
apply_workspace_settings() {
    # Allow override via environment variable
    ws="${GNOME_WORKSPACES:-9}"

    # Validate numeric input
    case "$ws" in
        *[!0-9]*|'') echo "[ERROR] Invalid workspace count: $ws" >&2; exit 1 ;;
    esac
    if [ "$ws" -lt 1 ]; then
        echo "[ERROR] Workspace count must be >= 1: $ws" >&2
        exit 1
    fi

    # Disable dynamic workspaces and set a fixed number
    gsettings_settings org.gnome.mutter dynamic-workspaces false
    gsettings_settings org.gnome.desktop.wm.preferences num-workspaces "$ws"
}

# Apply lock and idle settings (keep manual lock, disable only auto lock/blank)
apply_lock_settings() {
    # Disable automatic lock, but keep manual lock available
    gsettings_settings org.gnome.desktop.screensaver lock-enabled false
    gsettings_settings org.gnome.desktop.lockdown disable-lock-screen false

    # Disable idle blank (0 means never)
    gsettings_settings org.gnome.desktop.session idle-delay "uint32 0"

    # Ensure lock delay is zero if key exists
    gsettings_settings org.gnome.desktop.screensaver lock-delay "uint32 0"
}

# Apply dark mode settings
apply_dark_mode_settings() {
    # GNOME 42+ color-scheme
    gsettings_settings org.gnome.desktop.interface color-scheme prefer-dark
}

# Apply keyboard repeat settings
apply_keyboard_repeat_settings() {
    # Allow override via environment variables (milliseconds)
    delay="${KEY_REPEAT_DELAY_MS:-200}"
    interval="${KEY_REPEAT_INTERVAL_MS:-25}"

    # Validate numeric input
    case "$delay" in
        *[!0-9]*|'') echo "[ERROR] Invalid delay ms: $delay" >&2; exit 1 ;;
    esac
    case "$interval" in
        *[!0-9]*|'') echo "[ERROR] Invalid repeat interval ms: $interval" >&2; exit 1 ;;
    esac

    # Enable repeat and set timings
    gsettings_settings org.gnome.desktop.peripherals.keyboard repeat true
    gsettings_settings org.gnome.desktop.peripherals.keyboard delay "uint32 $delay"
    gsettings_settings org.gnome.desktop.peripherals.keyboard repeat-interval "uint32 $interval"
}

# ----- dconf helpers -----------------------------------------------------

# Load dconf subtree from file and verify
dconf_load_settings() {
    # Args: DC_PATH FILE
    dc_path="$1"
    dc_file="$2"

    # Ensure the source file exists
    if [ ! -r "$dc_file" ]; then
        echo "[ERROR] dconf source not found: $dc_file" >&2
        exit 1
    fi

    echo "[INFO] Loading dconf path $dc_path from $dc_file"
    if ! dconf load "$dc_path" < "$dc_file"; then
        echo "[ERROR] dconf load failed for $dc_path" >&2
        exit 1
    fi

    # Dump after load to confirm the subtree is not empty
    after_dump="$(dconf dump "$dc_path" 2>/dev/null)"
    if [ -z "$after_dump" ]; then
        echo "[ERROR] dconf dump is empty after load for $dc_path" >&2
        exit 1
    fi

    # Confirm all key=value lines in the source file exist in the loaded dump
    # - Skip blank lines, section headers ([...]), and comments
    # - Compare full "key=value" lines (do not truncate values)
    expected=0
    applied=0
    while IFS= read -r line; do
        case "$line" in
            ""|\#*|\;*|\[*\]) continue ;;
        esac
        case "$line" in
            *=*)
                expected=$((expected + 1))
                printf "%s\n" "$after_dump" | grep -Fqx -e "$line" && {
                    applied=$((applied + 1))
                    continue
                }
                echo "[ERROR] dconf key not applied under $dc_path: $line" >&2
                exit 1
                ;;
            *) continue ;;
        esac
    done < "$dc_file"

    echo "[INFO] Confirming dconf $dc_path keys applied: expected=$expected applied=$applied"
    if [ "$expected" != "$applied" ]; then
        echo "[ERROR] dconf confirmation mismatch under $dc_path" >&2
        exit 1
    fi
}

# Import GNOME keybindings (media keys and WM bindings)
import_gnome_keybindings() {
    dconf_load_settings "/org/gnome/settings-daemon/plugins/media-keys/" "$SCRIPTS/etc/gnome/gnome-shortcuts.conf"
    dconf_load_settings "/org/gnome/desktop/wm/keybindings/" "$SCRIPTS/etc/gnome/gnome-wm-keys.conf"
}

# ----- other helpers -----------------------------------------------------

# Install xfce4-terminal profile
install_xfce4_terminal_profile() {
    dst="$HOME/.config/xfce4/terminal"
    src="$SCRIPTS/etc/xfce/terminalrc"

    # Ensure xfce4-terminal exists
    if ! command -v xfce4-terminal >/dev/null 2>&1; then
        echo "[WARN] xfce4-terminal not found. Skipping terminal profile installation."
        return 0
    fi

    # Ensure the source profile exists
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

# Mask a user unit safely (no output unless failure)
mask_user_service() {
    # Args: UNIT
    unit="$1"

    echo "[INFO] Masking user unit: $unit"
    systemctl --user mask --now "$unit" >/dev/null 2>&1 || {
        echo "[ERROR] Failed to mask user unit: $unit" >&2
        exit 1
    }
}

# Apply service masks for a server-like desktop usage
disable_services() {
    # Skip only when DISABLE_SERVICES=no (default is yes)
    if [ "${DISABLE_SERVICES:-yes}" != "yes" ]; then
        echo "[INFO] Skip service disable/mask steps (set DISABLE_SERVICES=no to skip)."
        return 0
    fi

    # Disable search/indexer services
    mask_user_service localsearch-3.service
    mask_user_service tracker-miner-fs-3.service
    mask_user_service tracker-extract-3.service
    mask_user_service tracker-xdg-portal-3.service
    mask_user_service tracker-miner-rss-3.service

    # Disable online accounts / volume monitor integration
    mask_user_service gvfs-goa-volume-monitor.service
    mask_user_service goa-daemon.service
    mask_user_service goa-identity-service.service

    # Disable Evolution Data Server (Mail/Calendar backend)
    mask_user_service evolution-source-registry.service
    mask_user_service evolution-addressbook-factory.service
    mask_user_service evolution-calendar-factory.service

    # Disable DLNA media sharing
    mask_user_service rygel.service

    echo "[INFO] Service mask steps applied."
}

# Ask user whether to apply settings
confirm_apply_settings() {
    echo "[INFO] This script will apply GNOME Shell settings to the current user session."
    echo "[INFO] It will apply gsettings/dconf changes, optionally install a terminal profile, and may mask user services."
    printf "[INFO] Proceed to apply settings now? [y/N]: "
    read -r response < /dev/tty

    case "$response" in
        y|Y|yes|YES)
            echo "[INFO] Proceeding..."
            ;;
        *)
            echo "[INFO] Aborted."
            exit 1
            ;;
    esac
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_scripts
    check_session_bus

    # Verify required commands for this script
    check_commands gsettings dconf mkdir cp awk uname wc tr grep systemctl

    confirm_apply_settings

    # Apply GNOME Shell settings
    apply_media_handling_settings
    apply_workspace_settings
    apply_lock_settings
    apply_dark_mode_settings
    apply_keyboard_repeat_settings

    # Install profiles and autostart entries
    install_xfce4_terminal_profile

    # Apply GNOME keybindings
    import_gnome_keybindings

    # Mask background services (set DISABLE_SERVICES=no to skip)
    disable_services

    echo "[INFO] GNOME settings have been updated successfully."
    return 0
}

# Execute main function
main "$@"

#!/bin/sh

########################################################################
# debian_xfce_setup.sh: Apply Xfce desktop settings
#
#  Description:
#  Apply Xfce desktop settings:
#    - Media handling: disable automount in thunar-volman
#    - Desktop & workspaces: hide desktop icons; set workspace_count=9
#    - WM keybindings (xfwm4):
#        * Switch to workspace 1..9 with <Primary>1..9
#        * Move window to workspace 1..9 with <Primary><Alt>1..9
#        * Switch/move by direction with <Primary><Alt>Arrow and <Shift><Primary><Alt>Arrow
#        * Maximize window with <Primary><Alt>z
#    - Media keys and custom app shortcuts (xfce4-keyboard-shortcuts):
#        * Manual lock <Primary><Alt>l via xflock4
#        * Screenshot <Primary><Alt>s (full), <Primary><Alt>a (region)
#        * Open WWW <Primary><Alt>f
#        * Custom launchers: xfce4-terminal (C-x), emacs, thunar (C-t), vmware, gthumb
#        * Settings Manager <Primary><Alt>w
#    - Lock & idle behavior: keep manual lock, disable auto lock/blank
#    - Appearance: prefer dark color scheme; set GTK theme Adwaita-dark when available
#    - Panels: remove panel-2 and keep panel-1 only
#    - Wallpaper: set to none (no image)
#    - Profiles & autostart: install xfce4-terminal profile; install xmodmap autostart entry
#                            install xset-rate autostart entry
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./debian_xfce_setup.sh
#
#  Notes:
#  - Run inside a logged in desktop user session because xfconf (and tools)
#    require a DBus session bus.
#  - SCRIPTS environment variable must point to the root of this repo to load
#    terminal profile and autostart entries.
#
#  Error Conditions:
#  - If required commands are missing, execution is halted.
#  - If DBus session is not available, execution is halted.
#
#  Version History:
#  v1.0 2025-09-04
#       Initial version based on debian_gnome_flashback_setup.sh structure.
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

# Check if a desktop environment is installed (Debian/Ubuntu)
check_desktop_installed() {
    # Prefer tasksel if available
    if command -v tasksel >/dev/null 2>&1; then
        if LC_ALL=C tasksel --list-tasks | grep -q '^i.*desktop'; then
            echo "[INFO] Desktop environment detected via tasksel."
            return 0
        fi
    fi

    # Fallback: check for session files
    if [ -d /usr/share/xsessions ] && ls /usr/share/xsessions/*.desktop >/dev/null 2>&1; then
        echo "[INFO] Desktop environment detected via /usr/share/xsessions."
        return 0
    fi
    if [ -d /usr/share/wayland-sessions ] && ls /usr/share/wayland-sessions/*.desktop >/dev/null 2>&1; then
        echo "[INFO] Desktop environment detected via /usr/share/wayland-sessions."
        return 0
    fi

    echo "[ERROR] No desktop environment detected." >&2
    exit 1
}

# Ensure we have a user session DBus (required for xfconf)
check_session_bus() {
    if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
        echo "[ERROR] DBUS session bus is not available." >&2
        echo "Run this script inside a logged-in desktop session." >&2
        exit 1
    fi
}

# ----- xfconf helpers ----------------------------------------------------

# Set an xfconf key (bool) and confirm value
xfconf_settings_bool() {
    # Args: CHANNEL KEY VALUE(true|false)
    channel="$1"
    key="$2"
    value="$3"

    echo "[INFO] Setting: $channel $key -> $value"
    if ! xfconf-query -c "$channel" -p "$key" -s "$value" >/dev/null 2>&1; then
        if ! xfconf-query -c "$channel" -p "$key" -n -t bool -s "$value"; then
            echo "[ERROR] Failed to create $channel $key" >&2
            exit 1
        fi
    fi

    printf "%s" "[INFO] Confirming: $channel $key = "
    if ! xfconf-query -c "$channel" -p "$key"; then
        echo "[ERROR] Failed to read back $channel $key" >&2
        exit 1
    fi
}

# Set an xfconf key (int) and confirm value
xfconf_settings_int() {
    # Args: CHANNEL KEY VALUE(int)
    channel="$1"
    key="$2"
    value="$3"

    case "$value" in
        *[!0-9]*|'') echo "[ERROR] Invalid int value: $value" >&2; exit 1 ;;
    esac

    echo "[INFO] Setting: $channel $key -> $value"
    if ! xfconf-query -c "$channel" -p "$key" -s "$value" >/dev/null 2>&1; then
        if ! xfconf-query -c "$channel" -p "$key" -n -t int -s "$value"; then
            echo "[ERROR] Failed to create $channel $key" >&2
            exit 1
        fi
    fi

    printf "%s" "[INFO] Confirming: $channel $key = "
    if ! xfconf-query -c "$channel" -p "$key"; then
        echo "[ERROR] Failed to read back $channel $key" >&2
        exit 1
    fi
}

# Set an xfconf key (string) and confirm value
xfconf_settings_string() {
    # Args: CHANNEL KEY VALUE(string)
    channel="$1"
    key="$2"
    value="$3"

    echo "[INFO] Setting: $channel $key -> $value"
    if ! xfconf-query -c "$channel" -p "$key" -s "$value" >/dev/null 2>&1; then
        if ! xfconf-query -c "$channel" -p "$key" -n -t string -s "$value"; then
            echo "[ERROR] Failed to create $channel $key" >&2
            exit 1
        fi
    fi

    printf "%s" "[INFO] Confirming: $channel $key = "
    if ! xfconf-query -c "$channel" -p "$key"; then
        echo "[ERROR] Failed to read back $channel $key" >&2
        exit 1
    fi
}

# ----- apply media handling settings -------------------------------------

# Apply media handling settings
apply_media_handling_settings() {
    xfconf_settings_bool thunar-volman /automount-drives false
    xfconf_settings_bool thunar-volman /automount-media  false
}

# ----- apply UI settings (desktop icons, workspaces, wallpaper) ----------

# Apply UI settings (desktop icons, workspaces)
apply_ui_settings() {
    # Hide desktop icons
    xfconf_settings_int  xfce4-desktop /desktop-icons/style 0
    xfconf_settings_bool xfce4-desktop /desktop-icons/file-icons/show-home  false
    xfconf_settings_bool xfce4-desktop /desktop-icons/file-icons/show-trash false

    # Set 9 workspaces
    xfconf_settings_int xfwm4 /general/workspace_count 9
}

# Set wallpaper to none (no image)
apply_wallpaper_none() {
    echo "[INFO] Setting wallpaper to none"
    # Clear image-path/last-image across all monitors/workspaces if present
    for p in $(xfconf-query -c xfce4-desktop -l 2>/dev/null | grep '/backdrop/' | grep -E '(image-path|last-image)$'); do
        xfconf_settings_string xfce4-desktop "$p" ""
    done
    # Disable image-show if keys exist
    for p in $(xfconf-query -c xfce4-desktop -l 2>/dev/null | grep '/backdrop/' | grep 'image-show$'); do
        xfconf_settings_bool xfce4-desktop "$p" false
    done
    # Set image-style to 0 (None) for all monitors
    for p in $(xfconf-query -c xfce4-desktop -l 2>/dev/null | grep '/backdrop/' | grep 'image-style$'); do
        xfconf_settings_int xfce4-desktop "$p" 0
    done
}

# ----- apply lock and idle settings -------------------------------------

# Apply lock and idle settings (keep manual lock, disable only auto lock/blank)
apply_lock_settings() {
    # Disable automatic lock/saver if xfce4-screensaver exists
    if command -v xfce4-screensaver >/dev/null 2>&1; then
        xfconf_settings_bool xfce4-screensaver /lock/enabled  false
        xfconf_settings_bool xfce4-screensaver /saver/enabled false
    else
        echo "[INFO] xfce4-screensaver not found; skipping screensaver keys."
    fi

    # Idling/blanking is handled by xset-rate autostart (installed below)
    echo "[INFO] Skipping idle blank configuration here (handled by xset-rate autostart)."
}

# ----- apply dark mode ---------------------------------------------------

# Apply dark mode
apply_dark_mode_settings() {
    xfconf_settings_string xsettings /Net/ThemeName     "Adwaita-dark"
    xfconf_settings_string xsettings /Net/IconThemeName "Adwaita"
}

# ----- panel settings ----------------------------------------------------

# Remove panel-2 and keep panel-1
apply_panel_settings() {
    if xfconf-query -c xfce4-panel -p /panels/panel-2 -v >/dev/null 2>&1; then
        echo "[INFO] Removing xfce4-panel panel-2"
        if ! xfconf-query -c xfce4-panel -p /panels/panel-2 -r -R; then
            echo "[WARN] Failed to remove /panels/panel-2 subtree" >&2
        fi
    else
        echo "[INFO] panel-2 not present; nothing to remove"
    fi
}

# ----- import Xfce keybindings ------------------------------------------

# Import Xfce keybindings (media keys and WM bindings)
import_xfce_keybindings() {
    # WM: Switch to/move to workspace 1..9
    i=1
    while [ "$i" -le 9 ]; do
        xfconf_settings_string xfwm4 "/general/activate_workspace_${i}_key"    "<Primary>${i}"
        xfconf_settings_string xfwm4 "/general/move_window_workspace_${i}_key" "<Primary><Alt>${i}"
        i=$((i+1))
    done

    # WM: Switch/move by direction
    xfconf_settings_string xfwm4 /general/cycle_workspaces_key       "<Primary><Alt>Right"
    xfconf_settings_string xfwm4 /general/cycle_workspaces_prev_key  "<Primary><Alt>Left"
    xfconf_settings_string xfwm4 /general/move_window_left_workspace_key  "<Shift><Primary><Alt>Left"
    xfconf_settings_string xfwm4 /general/move_window_right_workspace_key "<Shift><Primary><Alt>Right"

    # WM: Maximize
    xfconf_settings_string xfwm4 /general/maximize_window_key "<Primary><Alt>z"

    # Custom app shortcuts (C-x terminal, C-t thunar = spec)
    ch="xfce4-keyboard-shortcuts"
    xfconf_settings_string "$ch" "/commands/custom/<Primary><Alt>x" "xfce4-terminal"
    xfconf_settings_string "$ch" "/commands/custom/<Primary><Alt>t" "thunar"
    xfconf_settings_string "$ch" "/commands/custom/<Primary><Alt>e" "emacs"
    xfconf_settings_string "$ch" "/commands/custom/<Primary><Alt>f" "exo-open --launch WebBrowser"
    xfconf_settings_string "$ch" "/commands/custom/<Primary><Alt>g" "gthumb"
    xfconf_settings_string "$ch" "/commands/custom/<Primary><Alt>v" "vmware"
    xfconf_settings_string "$ch" "/commands/custom/<Primary><Alt>w" "xfce4-settings-manager"
    xfconf_settings_string "$ch" "/commands/custom/<Primary><Alt>s" "xfce4-screenshooter -f"
    xfconf_settings_string "$ch" "/commands/custom/<Primary><Alt>a" "xfce4-screenshooter -r"
    xfconf_settings_string "$ch" "/commands/custom/<Primary><Alt>l" "xflock4"
}

# ----- other helpers -----------------------------------------------------

# Install xfce4-terminal profile
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

# Install autostart entry to load Xmodmap at login
install_xmodmap_autostart() {
    dst="$HOME/.config/autostart"
    src="$SCRIPTS/dot_files/dot_config/autostart/xmodmap.desktop"

    if [ ! -r "$src" ]; then
        echo "[ERROR] xmodmap.desktop not found: $src" >&2
        exit 1
    fi

    echo "[INFO] Installing xmodmap autostart entry to $dst"
    if ! mkdir -p "$dst"; then
        echo "[ERROR] Failed to create directory: $dst" >&2
        exit 1
    fi

    if ! cp "$src" "$dst/"; then
        echo "[ERROR] Failed to copy xmodmap.desktop to $dst" >&2
        exit 1
    fi

    if ! chmod 0644 "$dst/xmodmap.desktop"; then
        echo "[ERROR] Failed to set permissions on $dst/xmodmap.desktop" >&2
        exit 1
    fi

    echo "[INFO] xmodmap autostart entry installed"
}

# Install autostart entry to run xset at login
install_xset_autostart() {
    dst="$HOME/.config/autostart"
    src="$SCRIPTS/dot_files/dot_config/autostart/xset-rate.desktop"

    if [ ! -r "$src" ]; then
        echo "[ERROR] xset-rate.desktop not found: $src" >&2
        exit 1
    fi

    echo "[INFO] Installing xset-rate autostart entry to $dst"
    if ! mkdir -p "$dst"; then
        echo "[ERROR] Failed to create directory: $dst" >&2
        exit 1
    fi

    if ! cp "$src" "$dst/"; then
        echo "[ERROR] Failed to copy xset-rate.desktop to $dst" >&2
        exit 1
    fi

    if [ ! -f "$dst/xset-rate.desktop" ]; then
        echo "[ERROR] Copied file not found at $dst/xset-rate.desktop" >&2
        exit 1
    fi

    if ! chmod 0644 "$dst/xset-rate.desktop"; then
        echo "[ERROR] Failed to set permissions on $dst/xset-rate.desktop" >&2
        exit 1
    fi

    echo "[INFO] xset-rate autostart entry installed"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_scripts
    check_session_bus
    check_desktop_installed
    check_commands xfconf-query mkdir cp awk chmod uname grep ls wc tr

    apply_media_handling_settings
    apply_ui_settings
    apply_lock_settings
    apply_dark_mode_settings
    install_xfce4_terminal_profile
    install_xmodmap_autostart
    install_xset_autostart
    import_xfce_keybindings

    echo "[INFO] Xfce settings have been updated successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

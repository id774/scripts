#!/bin/sh

########################################################################
# debian_gnome_flashback_setup.sh: Apply GNOME settings for Flashback session
#
#  Description:
#  Apply GNOME settings used with Flashback. This script:
#    - Media handling: disable automount/automount-open and set autorun-never=true
#    - Desktop & workspaces: hide desktop icons; set org.gnome.desktop.wm.preferences num-workspaces=9
#    - WM keybindings (via dconf load, gnome-wm-keys.conf):
#        * Switch to workspace 1..9 with <Primary>1..9
#        * Move window to workspace 1..9 with <Primary><Alt>1..9
#        * Switch/move by direction with <Control><Alt>Arrow and <Shift><Control><Alt>Arrow
#        * Disable/adjust other window actions as defined (close/minimize/switch-windows etc.)
#    - Media keys and custom app shortcuts (via dconf load, gnome-shortcuts.conf):
#        * Screensaver <Primary><Alt>l, screenshot <Primary><Alt>s and area variants
#        * Open WWW <Primary><Alt>f
#        * Custom launchers: xfce4-terminal, emacs, thunar, vmplayer, gthumb
#    - Lock & idle behavior: keep manual lock, disable auto lock; set idle-delay=0 and lock-delay=0
#    - Appearance: prefer dark color scheme and set Adwaita-dark when available
#    - Keyboard repeat: enable repeat; default delay=200ms and interval=25ms (overridable via env)
#    - Profiles & autostart: install xfce4-terminal profile; install xmodmap autostart entry
#    - Optional: reset gnome-panel to defaults on user confirmation
#    - Confirm before applying settings and before optional gnome-panel reset
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./debian_gnome_flashback_setup.sh
#
#  Notes:
#  - Run inside a logged in desktop user session because gsettings and dconf
#    require a DBus session bus.
#  - SCRIPTS environment variable must point to the root of this repo to load
#    keybinding files and terminal profile.
#
#  Error Conditions:
#  - If required commands are missing, execution is halted.
#  - If DBus session is not available, execution is halted.
#
#  Version History:
#  v2.3 2026-02-26
#       Prompt for confirmation before applying settings.
#  v2.2 2026-02-14
#       Improve dconf verification logic to compare full key=value entries and ensure required grep dependency.
#  v2.1 2025-08-20
#       Expanded header documentation to enumerate all applied configuration
#       steps and system tuning tasks for consistency and transparency.
#  v2.0 2025-08-12
#       Replace previous LightDM oriented setup with GNOME settings only.
#       Keep POSIX compliance and robust logging and verification.
#       Add GNOME settings hook with safe guards and logging.
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-13
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

    if ! gsettings_can_set "$schema" "$key"; then
        echo "[WARN] Skipping unknown or read-only key: $schema $key" >&2
        return 0
    fi

    echo "[INFO] Setting: $schema $key -> $value"
    if ! gsettings set "$schema" "$key" "$value"; then
        echo "[ERROR] Failed to set $schema $key to $value" >&2
        exit 1
    fi

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

# Apply UI settings (desktop icons, workspaces)
apply_ui_settings() {
    gsettings_settings org.gnome.desktop.background show-desktop-icons false
    gsettings_settings org.gnome.desktop.wm.preferences num-workspaces 9
}

# Apply lock and idle settings (keep manual lock, disable only auto lock/blank)
apply_lock_settings() {
    # Disable automatic lock, but keep manual lock available
    gsettings_settings org.gnome.desktop.screensaver lock-enabled false
    gsettings_settings org.gnome.desktop.lockdown disable-lock-screen false
    # Disable idle blank (0 means never)
    gsettings_settings org.gnome.desktop.session idle-delay "uint32 0"
    # If delay key exists under screensaver, ensure minimal delay
    gsettings_settings org.gnome.desktop.screensaver lock-delay "uint32 0"
}

# Apply dark mode
apply_dark_mode_settings() {
    # GNOME 42+ color-scheme
    gsettings_settings org.gnome.desktop.interface color-scheme prefer-dark
    # Fallback to Adwaita-dark if available
    gsettings_settings org.gnome.desktop.interface gtk-theme Adwaita-dark
    # Optional: dark icon theme if desired; skipped unless you want it
    # gsettings_settings org.gnome.desktop.interface icon-theme Adwaita
}

# Apply keyboard repeat settings (high sensitivity)
apply_keyboard_repeat_settings() {
    # Allow override via environment variables (milliseconds)
    #   KEY_REPEAT_DELAY_MS: time before repeat starts (default 200)
    #   KEY_REPEAT_INTERVAL_MS: interval between repeats (default 25)
    delay="${KEY_REPEAT_DELAY_MS:-200}"
    interval="${KEY_REPEAT_INTERVAL_MS:-25}"

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

    # Ensure readable permissions for .desktop entry
    if ! chmod 0644 "$dst/xmodmap.desktop"; then
        echo "[ERROR] Failed to set permissions on $dst/xmodmap.desktop" >&2
        exit 1
    fi

    echo "[INFO] xmodmap autostart entry installed"
}

# Ask user whether to reset gnome-panel
reset_gnome_panel() {
    printf "[INFO] Do you want to reset gnome-panel? [y/N]: "
    read -r response < /dev/tty

    case "$response" in
        y|Y|yes|YES)
            echo "[INFO] Resetting gnome-panel..."
            dconf reset -f /org/gnome/gnome-panel/ || {
                echo "[ERROR] Failed to reset gnome-panel." >&2
                exit 1
            }
            ;;
        *)
            echo "[INFO] Skip reset."
            ;;
    esac
}

# Ask user whether to apply settings
confirm_apply_settings() {
    echo "[INFO] This script will apply GNOME settings for Flashback to the current user session."
    echo "[INFO] It will modify GNOME settings and install configuration files in your home directory."
    printf "[INFO] Proceed to apply settings now? [y/N]: "
    read -r response < /dev/tty

    case "$response" in
        y|Y|yes|YES)
            echo "[INFO] Proceeding..."
            ;;
        *)
            echo "[INFO] Aborted."
            exit 0
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
    check_desktop_installed
    check_commands gsettings dconf mkdir cp awk chmod uname grep ls wc tr

    confirm_apply_settings

    apply_media_handling_settings
    apply_ui_settings
    apply_lock_settings
    apply_dark_mode_settings
    apply_keyboard_repeat_settings
    install_xfce4_terminal_profile
    install_xmodmap_autostart
    import_gnome_keybindings
    reset_gnome_panel

    echo "[INFO] GNOME settings have been updated successfully."
    return 0
}

# Execute main function
main "$@"

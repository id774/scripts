#!/bin/sh

########################################################################
# debian_desktop_setup.sh: Debian batch setup script for Desktop
#
#  Description:
#  Configure essential settings for Debian-based desktop environments.
#  Optionally disable guest sessions in LightDM and restart it when present.
#  Optionally run GNOME gsettings configuration for Flashback session.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./debian_desktop_setup.sh
#
#  Notes:
#  - Designed for Debian-based desktops. LightDM is optional.
#  - If LightDM is not installed, LightDM-related steps are skipped with a warning.
#  - Desktop Environment detection is best effort and non-fatal on minimal installs.
#  - Manual verification of /etc/lightdm/lightdm.conf may be required when LightDM exists.
#  - Review and modify configurations as needed before execution.
#
#  Error Conditions:
#  - If required commands are missing, execution is halted.
#  - Errors from underlying commands should be resolved based on their output.
#
#  Version History:
#  v1.4 2025-08-11
#       Make LightDM optional and skip related steps when absent.
#       Add GNOME settings hook with safe guards and logging.
#       Keep POSIX compliance and unify log format.
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-13
#       Automated guest session disabling in LightDM.
#       Improved script automation and removed manual editing step.
#       Added system checks and improved error handling.
#       Redirected error messages to stderr for better logging and debugging.
#  [Further version history truncated for brevity]
#  v0.1 2011-09-28
#       First version.
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

# Track LightDM availability: 0 unknown, 1 present, 2 absent
LIGHTDM_STATE=0

# Check if the system is Linux
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

# Check if required commands are available and executable
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

# Check if a desktop environment is installed (non-fatal, best effort)
check_desktop_installed() {
    # 1) Session files
    if [ -d /usr/share/xsessions ] && ls /usr/share/xsessions/*.desktop >/dev/null 2>&1; then
        echo "[INFO] Desktop environment detected via /usr/share/xsessions."
        return 0
    fi
    if [ -d /usr/share/wayland-sessions ] && ls /usr/share/wayland-sessions/*.desktop >/dev/null 2>&1; then
        echo "[INFO] Desktop environment detected via /usr/share/wayland-sessions."
        return 0
    fi
    # 2) Common packages on Debian/Ubuntu families
    PKGS="ubuntu-desktop gnome-shell gnome-session-bin gnome-flashback xfce4-session xfce4 plasma-desktop kde-standard kde-plasma-desktop mate-session-manager cinnamon lxqt-session"
    for p in $PKGS; do
        if dpkg-query -W -f='${Status}' "$p" 2>/dev/null | grep -q "ok installed"; then
            echo "[INFO] Desktop environment detected via package: $p"
            return 0
        fi
    done
    # 3) Optional: tasksel hint if available
    if command -v tasksel >/dev/null 2>&1; then
        if tasksel --list-tasks 2>/dev/null | grep -Eiq '(ubuntu|kubuntu|xubuntu|lubuntu).*-desktop|desktop'; then
            echo "[INFO] Desktop environment hinted by tasksel."
            return 0
        fi
    fi
    echo "[WARN] No desktop environment detected. Continuing setup with LightDM and GNOME steps as applicable." >&2
    return 1
}

# Check if LightDM is installed (non-fatal)
check_lightdm() {
    if dpkg-query -W -f='${Status}' lightdm 2>/dev/null | grep -q "ok installed"; then
        LIGHTDM_STATE=1
        echo "[INFO] LightDM detected."
    else
        LIGHTDM_STATE=2
        echo "[WARN] LightDM is not installed. LightDM related steps will be skipped." >&2
    fi
}

# Configure GNOME media and UI settings (runs only if helper exists)
setup_gsettings() {
    if [ -z "$SCRIPTS" ]; then
        echo "[WARN] Skipping GNOME settings because SCRIPTS is not set." >&2
        return 0
    fi
    GSCRIPT="$SCRIPTS/installer/setup_gsettings.sh"
    if [ ! -x "$GSCRIPT" ]; then
        echo "[WARN] Skipping GNOME settings because script not found or not executable: $GSCRIPT" >&2
        return 0
    fi
    # Let the helper handle DBus session checks by itself
    echo "[INFO] Running GNOME settings setup: $GSCRIPT"
    "$GSCRIPT" || {
        echo "[WARN] GNOME settings setup returned a non zero status." >&2
        return 0
    }
}

# Disable guest sessions in LightDM
disable_guest_session() {
    if [ "$LIGHTDM_STATE" -ne 1 ]; then
        echo "[INFO] Skipping LightDM guest session disable because LightDM is absent."
        return 0
    fi
    LIGHTDM_CONF="/etc/lightdm/lightdm.conf"
    if ! sudo mkdir -p /etc/lightdm; then
        echo "[WARN] Failed to create /etc/lightdm directory." >&2
        return 0
    fi
    if ! grep -q "^allow-guest=false" "$LIGHTDM_CONF" 2>/dev/null; then
        echo "[INFO] Disabling guest sessions in LightDM..."
        if ! echo "allow-guest=false" | sudo tee -a "$LIGHTDM_CONF" >/dev/null; then
            echo "[WARN] Failed to write LightDM config: $LIGHTDM_CONF" >&2
        fi
    else
        echo "[INFO] Guest sessions are already disabled in LightDM."
    fi
}

# Restart LightDM to apply changes
restart_lightdm() {
    if [ "$LIGHTDM_STATE" -ne 1 ]; then
        echo "[INFO] Skipping LightDM restart because LightDM is absent."
        return 0
    fi
    echo "[INFO] Restarting LightDM..."
    if ! sudo systemctl restart lightdm; then
        echo "[WARN] Failed to restart LightDM." >&2
    fi
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands sudo dpkg-query grep tee systemctl
    check_desktop_installed
    check_lightdm

    # GNOME settings can run regardless of LightDM presence
    setup_gsettings

    # Only require sudo and run LightDM steps when LightDM is present
    if [ "$LIGHTDM_STATE" -eq 1 ]; then
        check_sudo
        disable_guest_session
        restart_lightdm
    else
        echo "[INFO] LightDM steps skipped."
    fi

    echo "[INFO] All Debian desktop setup completed."
    return 0
}

# Execute main function
main "$@"
exit $?

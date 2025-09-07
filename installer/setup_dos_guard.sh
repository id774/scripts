#!/bin/sh

########################################################################
# setup_dos_guard.sh: Deploy & enable Fail2ban + mod_evasive for Apache2
#
#  Description:
#  Deploy prepared configs and enable protections in an idempotent way:
#    - Copy $SCRIPTS configs to system locations if changed
#    - Ensure mod_evasive log directory and permissions
#    - Ensure apache-evasive fail2ban filter exists
#    - Enable Apache module and reload services
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#     ./setup_dos_guard.sh
#     (use -h / --help to print this header)
#
#  Notes:
#  - Before execution, the script now asks for confirmation because it will
#    modify Apache and fail2ban configurations, create log directories, and
#    reload services. This prevents accidental or unintended execution.
#  - Expected files under $SCRIPTS:
#     $SCRIPTS/etc/apache/mods-available/evasive.conf
#     $SCRIPTS/etc/fail2ban/jail.local
#     (optional) $SCRIPTS/etc/fail2ban/filter.d/apache-evasive.conf
#
#  Requirements:
#   - Linux (Debian-family assumed)
#   - sudo privileges (script invokes sudo)
#   - Commands: sudo, cmp, cp, chmod, chown, mkdir, a2enmod, apachectl,
#               systemctl, fail2ban-client
#
#  Version History:
#  v1.1 2025-09-07
#       Add interactive confirmation prompt before applying configurations.
#  v1.0 2025-08-27
#       Initial release.
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
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if the SCRIPTS variable is unset or empty
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the path of your script collection." >&2
        exit 1
    fi
}

# Check if required commands are available and executable before execution
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Resolve and validate source paths from $SCRIPTS
resolve_sources() {
    # Determines absolute source paths for provided configs.
    EVASIVE_SRC="$SCRIPTS/etc/apache/mods-available/evasive.conf"
    JAIL_LOCAL_SRC="$SCRIPTS/etc/fail2ban/jail.local"
    FILTER_SRC="$SCRIPTS/etc/fail2ban/filter.d/apache-evasive.conf" # optional

    if [ ! -f "$EVASIVE_SRC" ]; then
        echo "[ERROR] Missing config: $EVASIVE_SRC" >&2
        exit 1
    fi
    if [ ! -f "$JAIL_LOCAL_SRC" ]; then
        echo "[ERROR] Missing config: $JAIL_LOCAL_SRC" >&2
        exit 1
    fi
}

# Copy a file only if content differs (idempotent deployment)
install_if_changed() {
    # $1: src, $2: dest, $3: mode (e.g., 0644), $4: owner:group
    src="$1"; dest="$2"; mode="$3"; owner="$4"
    if [ -f "$dest" ] && cmp -s "$src" "$dest"; then
        echo "[INFO] Up-to-date: $dest"
        return 0
    fi
    echo "[INFO] Installing: $dest"
    sudo cp "$src" "$dest" || { echo "[ERROR] Failed to copy to $dest" >&2; exit 1; }
    sudo chown "$owner" "$dest" || { echo "[ERROR] chown failed for $dest" >&2; exit 1; }
    sudo chmod "$mode" "$dest" || { echo "[ERROR] chmod failed for $dest" >&2; exit 1; }
    return 0
}

# Deploy fail2ban's jail.local and ensure apache-evasive filter exists
deploy_fail2ban() {
    # Installs jail.local and creates/installs apache-evasive filter if needed.
    echo "[INFO] Deploying fail2ban configs"
    install_if_changed "$JAIL_LOCAL_SRC" "/etc/fail2ban/jail.local" 644 "root:root"

    dest="/etc/fail2ban/filter.d/apache-evasive.conf"
    if [ -f "$FILTER_SRC" ]; then
        install_if_changed "$FILTER_SRC" "$dest" 644 "root:root"
    elif [ ! -f "$dest" ]; then
        echo "[INFO] Creating default apache-evasive filter: $dest"
        tmp="/tmp/apache-evasive.$$"
        # Simple filter matching mod_evasive error lines with <HOST> placeholder.
        cat >"$tmp" <<'EOF'
[Definition]
failregex = \[evasive20:error\] \[pid .*?\] \[client <HOST>.*\] client denied by server configuration
ignoreregex =
EOF
        sudo cp "$tmp" "$dest" && sudo chown root:root "$dest" && sudo chmod 644 "$dest" || {
            echo "[ERROR] Failed to create $dest" >&2; rm -f "$tmp"; exit 1;
        }
        rm -f "$tmp"
    else
        echo "[INFO] Keeping existing filter: $dest"
    fi
}

# Deploy Apache mod_evasive config and prepare its log directory
deploy_evasive() {
    # Installs the evasive.conf and ensures log directory ownership/perms.
    echo "[INFO] Deploying Apache mod_evasive config"
    install_if_changed "$EVASIVE_SRC" "/etc/apache2/mods-available/evasive.conf" 644 "root:root"

    echo "[INFO] Ensuring /var/log/apache2/evasive exists"
    if ! sudo test -d /var/log/apache2/evasive; then
        sudo mkdir -p /var/log/apache2/evasive || { echo "[ERROR] Failed to create log dir" >&2; exit 1; }
    fi
    sudo chown www-data:adm /var/log/apache2/evasive || { echo "[ERROR] Failed to chown log dir" >&2; exit 1; }
    sudo chmod 750 /var/log/apache2/evasive || { echo "[ERROR] Failed to chmod log dir" >&2; exit 1; }
}

# Enable Apache module "evasive" and validate configuration, then reload
enable_and_reload_apache() {
    # Enables mod_evasive (idempotent) and reloads Apache if config is valid.
    echo "[INFO] Enabling Apache module: evasive"
    sudo a2enmod evasive >/dev/null 2>&1 || true

    echo "[INFO] Validating Apache configuration"
    if ! sudo apachectl -t; then
        echo "[ERROR] Apache config test failed" >&2
        exit 1
    fi

    echo "[INFO] Reloading Apache"
    if ! sudo systemctl reload apache2; then
        if ! sudo /etc/init.d/apache2 reload; then
            echo "[ERROR] Failed to reload Apache2" >&2
            exit 1
        fi
    fi
}

# Enable and (re)start fail2ban, then reload its configuration
enable_and_reload_fail2ban() {
    # Ensures fail2ban service is enabled and picks up new configs.
    echo "[INFO] Enabling and starting fail2ban"
    sudo systemctl enable --now fail2ban >/dev/null 2>&1 || true

    echo "[INFO] Reloading fail2ban"
    if ! sudo fail2ban-client reload; then
        echo "[ERROR] fail2ban reload reported an issue; see /var/log/fail2ban.log" >&2
        exit 1
    fi
}

# Print a short, helpful summary for quick verification
show_summary() {
    # Displays minimal service status and jail list.
    echo "---- Status summary ----"
    sudo systemctl --no-pager --full status apache2 2>/dev/null | sed -n '1,5p'
    sudo systemctl --no-pager --full status fail2ban 2>/dev/null | sed -n '1,5p'
    echo
    sudo fail2ban-client status 2>/dev/null || true
    echo
    sudo fail2ban-client status apache-evasive 2>/dev/null || true
}

# Ask for confirmation before execution
confirm_execution() {
    echo "[INFO] This script will deploy and enable DoS protection using Fail2ban and Apache mod_evasive."
    echo "[INFO] It modifies Apache and fail2ban configurations, creates log directories, and reloads services."
    echo "[INFO] Confirmation is required to prevent accidental or unintended execution."
    printf "[INFO] Do you want to proceed? [y/N]: "
    read -r response < /dev/tty

    case "$response" in
        y|Y|yes|YES) return 0 ;;
        *) echo "[ERROR] Aborted by user."; exit 1 ;;
    esac
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_scripts
    check_commands cmp cp chmod chown mkdir a2enmod apachectl systemctl fail2ban-client
    check_sudo
    resolve_sources

    # Ask for confirmation before proceeding
    confirm_execution

    echo "[INFO] Starting idempotent deployment"
    deploy_fail2ban
    deploy_evasive
    enable_and_reload_apache
    enable_and_reload_fail2ban
    show_summary
    echo "[INFO] Deployment completed successfully."
    exit 0
}

# Execute main function
main "$@"
exit $?

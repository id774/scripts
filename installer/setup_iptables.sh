#!/bin/sh

########################################################################
# setup_iptables.sh: Apply and enable default iptables rules on Debian
#
#  Description:
#  This script installs iptables-persistent if necessary, applies a
#  predefined rules.v4 template, loads the rules into the current kernel,
#  and ensures the rules are automatically restored on boot via
#  netfilter-persistent. It is designed for use on Debian-based systems.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-04-25
#       Exit immediately on failure in load_rules or enable_restore.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-27
#       Improving iptables restore instruction.
#  v1.0 2025-03-26
#       Initial release.
#
#  Usage:
#  Run the script directly:
#      ./setup_iptables.sh
#
#  Notes:
#  - Expects the environment variable SCRIPTS to point to the base path
#    containing etc/iptables/rules.v4.
#  - Will not overwrite /etc/iptables/rules.v4 if it already exists.
#  - Designed for Debian and Ubuntu systems using iptables.
#
#  Error Conditions:
#  - Exits if SCRIPTS is not set.
#  - Exits if required commands are missing.
#  - Exits if not running on Linux.
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
    if [ "$(uname -s)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if SCRIPTS environment variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please export SCRIPTS=/path/to/your/scripts before running this script." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Check for required commands
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

# Install iptables-persistent if not already installed
install_persistent() {
    if ! dpkg -s iptables-persistent >/dev/null 2>&1; then
        echo "[INFO] Installing iptables-persistent..."
        echo iptables-persistent iptables-persistent/autosave_v4 boolean true | sudo debconf-set-selections
        echo iptables-persistent iptables-persistent/autosave_v6 boolean false | sudo debconf-set-selections
        sudo apt-get update
        sudo apt-get install -y iptables-persistent
    else
        echo "[INFO] iptables-persistent already installed."
    fi
}

# Apply template if needed
apply_template_if_needed() {
    if [ ! -f "$RULES_PATH" ]; then
        echo "[INFO] Copying template to $RULES_PATH."
        sudo mkdir -p "$(dirname "$RULES_PATH")"
        sudo cp "$TEMPLATE_PATH" "$RULES_PATH"
    else
        echo "[INFO] $RULES_PATH already exists, skipping copy."
    fi
    sudo chmod 400 "$RULES_PATH"
}

# Load rules into the running kernel
load_rules() {
    echo "[INFO] Applying rules with iptables-restore."
    if ! sudo sh -c "iptables-restore < '$RULES_PATH'"; then
        echo "[ERROR] Failed to apply iptables rules with iptables-restore." >&2
        exit 1
    fi
}

# Ensure rules are restored on boot
enable_restore() {
    echo "[INFO] Restarting netfilter-persistent."
    if ! sudo systemctl restart netfilter-persistent; then
        echo "[ERROR] Failed to restart netfilter-persistent." >&2
        exit 1
    fi
}

# Print post-installation instructions and next steps
final_message() {
    echo ""
    echo "[INFO] iptables setup completed successfully."
    echo ""
    echo " You may want to review and edit the rules file:"
    echo "   sudo vi $RULES_PATH"
    echo ""
    echo " After editing, re-apply the rules with:"
    echo "   sudo sh -c 'iptables-restore < $RULES_PATH'"
    echo ""
}

# Main execution
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_scripts
    check_commands dpkg apt-get debconf-set-selections iptables-restore iptables-save sudo chmod mkdir cp systemctl
    check_sudo

    TEMPLATE_PATH="$SCRIPTS/etc/iptables/rules.v4"
    RULES_PATH="/etc/iptables/rules.v4"

    install_persistent
    apply_template_if_needed
    load_rules
    enable_restore
    final_message
    return 0
}

main "$@"
exit $?

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

# Check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "Error: This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if SCRIPTS environment variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set." >&2
        echo "Please export SCRIPTS=/path/to/your/scripts before running this script." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Check for required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Install iptables-persistent if not already installed
install_persistent() {
    if ! dpkg -s iptables-persistent >/dev/null 2>&1; then
        echo "Installing iptables-persistent..."
        echo iptables-persistent iptables-persistent/autosave_v4 boolean true | sudo debconf-set-selections
        echo iptables-persistent iptables-persistent/autosave_v6 boolean false | sudo debconf-set-selections
        sudo apt-get update
        sudo apt-get install -y iptables-persistent
    else
        echo "iptables-persistent already installed."
    fi
}

# Apply template if needed
apply_template_if_needed() {
    if [ ! -f "$RULES_PATH" ]; then
        echo "Copying template to $RULES_PATH"
        sudo mkdir -p "$(dirname "$RULES_PATH")"
        sudo cp "$TEMPLATE_PATH" "$RULES_PATH"
    else
        echo "$RULES_PATH already exists, skipping copy."
    fi
    sudo chmod 400 "$RULES_PATH"
}

# Load rules into the running kernel
load_rules() {
    echo "Applying rules with iptables-restore"
    sudo sh -c "iptables-restore < '$RULES_PATH'"
}

# Ensure rules are restored on boot
enable_restore() {
    echo "Restarting netfilter-persistent"
    sudo systemctl restart netfilter-persistent
}

# Print post-installation instructions and next steps
final_message() {
    echo ""
    echo "iptables setup completed successfully."
    echo ""
    echo "You may want to review and edit the rules file:"
    echo "  sudo vi $RULES_PATH"
    echo ""
    echo "After editing, re-apply the rules with:"
    echo "  sudo iptables-restore < $RULES_PATH"
    echo ""
}

# Main execution
main() {
    case "$1" in
        -h|--help) usage ;;
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
}

main "$@"

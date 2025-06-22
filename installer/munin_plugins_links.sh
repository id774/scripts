#!/bin/sh

########################################################################
# munin_plugins_links.sh: Automate Munin Plugin Configuration
#
#  Description:
#  This script automates the configuration of Munin plugins by generating
#  and executing a script with commands provided by munin-node-configure.
#  It also handles specific plugin removal and service restart. It checks
#  for the presence of Munin before proceeding.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.7 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.6 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.5 2025-03-17
#       Refactored to encapsulate all logic into functions and introduced main function.
#  v1.4 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.3 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.2 2024-08-11
#       Added checks for required directories and write permissions.
#       Added input validation to ensure directory is provided as an argument.
#       Display help message if no arguments are provided.
#  v1.1 2023-12-06
#       Added check for Munin installation.
#  v1.0 2019-08-16
#       Initial release.
#
#  Usage:
#      ./munin_plugins_links.sh [-h] [-c]
#
#  Options:
#  -h   Display this help message.
#  -c   Configure and link Munin plugins.
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

# Function to check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to check required commands
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

# Function to check if necessary directories exist and are writable
check_directories() {
    TMP_SCRIPT_DIR=${TMP:-/tmp}
    PLUGINS_DIR=/etc/munin/plugins

    if [ ! -w "$TMP_SCRIPT_DIR" ]; then
        echo "[ERROR] Temporary directory $TMP_SCRIPT_DIR is not writable." >&2
        exit 3
    fi

    if [ ! -d "$PLUGINS_DIR" ]; then
        echo "[ERROR] Plugins directory $PLUGINS_DIR does not exist." >&2
        exit 4
    fi
}

# Function to configure Munin plugins
configure_munin_plugins() {
    check_system
    check_sudo
    check_directories
    check_commands sudo munin-node-configure chmod rm systemctl

    TMP_SCRIPT_DIR=${TMP:-/tmp}
    SCRIPT_NAME=$TMP_SCRIPT_DIR/create-munin-plugins-links.sh
    PLUGINS_DIR=/etc/munin/plugins

    # Create a temporary script for Munin plugin setup
    echo "#!/bin/sh" > "$SCRIPT_NAME"
    sudo munin-node-configure --shell >> "$SCRIPT_NAME"
    chmod +x "$SCRIPT_NAME"

    # Execute the script in the plugins directory
    cd "$PLUGINS_DIR" || { echo "[ERROR] Failed to change directory to $PLUGINS_DIR." >&2; exit 5; }
    sudo "$SCRIPT_NAME"
    rm "$SCRIPT_NAME"

    # Remove specific plugins
    sudo rm ntp_[0-9]*

    # Restart Munin node service
    sudo systemctl restart munin-node.service

    echo "[INFO] All munin plugins setup completed."
}

# Main function to execute the script
main() {
    if [ "$#" -eq 0 ]; then
        usage
    fi

    while getopts "hc" opt; do
        case $opt in
            c)
                configure_munin_plugins
                ;;
            *)
                usage
                ;;
        esac
    done
    return 0
}

# Execute main function
main "$@"
exit $?

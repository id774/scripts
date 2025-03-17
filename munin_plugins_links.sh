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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
#  ./munin_plugins_links.sh [-h] [-c]
#
#  Options:
#  -h   Display this help message.
#  -c   Configure and link Munin plugins.
#
########################################################################

# Function to display help message
display_help() {
    cat << EOF
Usage: $0 [-h] [-c]

Description:
  This script automates the configuration of Munin plugins by generating
  and executing a script with commands provided by munin-node-configure.
  It also handles specific plugin removal and service restart.

Options:
  -h   Display this help message.
  -c   Configure and link Munin plugins.

EOF
}

# Function to check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "Error: This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Function to check if necessary directories exist and are writable
check_directories() {
    TMP_SCRIPT_DIR=${TMP:-/tmp}
    PLUGINS_DIR=/etc/munin/plugins

    if [ ! -w "$TMP_SCRIPT_DIR" ]; then
        echo "Error: Temporary directory $TMP_SCRIPT_DIR is not writable." >&2
        exit 3
    fi

    if [ ! -d "$PLUGINS_DIR" ]; then
        echo "Error: Plugins directory $PLUGINS_DIR does not exist." >&2
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
    cd "$PLUGINS_DIR" || { echo "Error: Failed to change directory to $PLUGINS_DIR." >&2; exit 5; }
    sudo "$SCRIPT_NAME"
    rm "$SCRIPT_NAME"

    # Remove specific plugins
    sudo rm ntp_[0-9]*

    # Restart Munin node service
    sudo systemctl restart munin-node.service
}

# Main function to parse options and execute corresponding actions
main() {
    if [ "$#" -eq 0 ]; then
        display_help
        exit 0
    fi

    while getopts "hc" opt; do
        case $opt in
            h)
                display_help
                exit 0
                ;;
            c)
                configure_munin_plugins
                ;;
            *)
                display_help
                exit 0
                ;;
        esac
    done
}

# Execute main function
main "$@"

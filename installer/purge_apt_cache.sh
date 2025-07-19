#!/bin/sh

########################################################################
# purge_apt_cache.sh: Clean Up Removed APT Packages
#
#  Description:
#  This script creates and executes a script to purge residual config
#  files of removed APT packages in Debian-based systems.
#
#  Notes:
#  - This script is intended for Debian-based systems only.
#  - It will permanently remove residual config files of APT packages.
#  - Ensure to review the script before running it.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script directly without any arguments:
#      ./purge_apt_cache.sh
#
#  Version History:
#  v1.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.3 2025-03-20
#       Added system and command check functions for improved security.
#       Structured script into functions for better modularity and error handling.
#  v1.2 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.1 2023-12-06
#       Added system check, refactored for clarity, and added notes.
#  v1.0 2019-08-29
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
    if [ "$(uname -s)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if the system is Debian-based
check_debian() {
    if [ ! -f /etc/debian_version ]; then
        echo "[ERROR] This script only runs on Debian-based systems." >&2
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

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v >/dev/null 2>&1; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Set temporary file location
set_temp_file() {
    SCRIPT_NAME="${TMP:-/tmp}/purge_apt_cache.sh"
}

# Generate and execute the cleanup script
perform_cleanup() {
    echo "#!/bin/sh" > "$SCRIPT_NAME"
    CONFIGS_TO_PURGE=$(aptitude search . | grep '^c' | awk '{print $2}')
    if [ -z "$CONFIGS_TO_PURGE" ]; then
        echo "[INFO] No residual config files to purge."
        exit 0
    fi
    echo "$CONFIGS_TO_PURGE" | sed 's/^/sudo apt purge -y /g' >> "$SCRIPT_NAME"
    chmod +x "$SCRIPT_NAME"
    echo "[INFO] The following packages will be purged:"
    echo "$CONFIGS_TO_PURGE"
    echo "[INFO] Do you want to continue? (y/n): "
    read REPLY
    if [ "$REPLY" = "y" ] || [ "$REPLY" = "Y" ]; then
        "$SCRIPT_NAME"
    fi
    rm "$SCRIPT_NAME"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_debian
    check_commands aptitude awk sed chmod cat rm
    check_sudo
    set_temp_file
    trap 'rm -f "$SCRIPT_NAME"' EXIT
    perform_cleanup

    echo "[INFO] Cleanup completed."
    return 0
}

# Execute main function
main "$@"
exit $?

#!/bin/sh

########################################################################
# clear_chromium_cache.sh: Clear Chromium Browser Cache
#
#  Description:
#  This script provides options to remove Chromium's "Web Data" directory.
#  It no longer forcefully terminates Chromium processes.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-03-17
#       Encapsulated all logic into functions and introduced main function.
#  v1.2 2024-08-11
#       Removed forceful termination of Chromium processes.
#       Added option to clear "Web Data" directory. Display help message by default.
#  v1.1 2023-12-06
#       Refactored for clarity, added notes, and renamed the script.
#  v1.0 2016-01-08
#       Initial release.
#
#  Usage:
#  ./clear_chromium_cache.sh -c
#
#  Options:
#  -h   Display this help message.
#  -c   Clear Chromium "Web Data" directory.
#
#  Notes:
#  - This script no longer forcefully terminates Chromium processes.
#  - Ensure that Chromium is closed before running with the -c option
#    to avoid potential data corruption.
#
########################################################################

# Function to display help message
display_help() {
    cat << EOF
Usage: $0 [-h] [-c]

Options:
  -h   Display this help message.
  -c   Clear Chromium "Web Data" directory.

Notes:
  - This script no longer forcefully terminates Chromium processes.
  - Ensure that Chromium is closed before running with the -c option
    to avoid potential data corruption.
EOF
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

# Function to clear Chromium "Web Data" directory
clear_cache() {
    cache_dir="$HOME/.config/chromium/Default/Web Data"

    if [ -e "$cache_dir" ]; then
        rm -rf "$cache_dir"
        echo "Cleared: $cache_dir"
        exit 0
    else
        echo "Not found: $cache_dir" >&2
        exit 1
    fi
}

# Function to parse command-line arguments
parse_arguments() {
    while getopts "hc" opt; do
        case $opt in
            h)
                display_help
                exit 0
                ;;
            c)
                clear_cache
                ;;
            *)
                display_help
                exit 0
                ;;
        esac
    done

    # If no options are provided, display help
    if [ $OPTIND -eq 1 ]; then
        display_help
        exit 0
    fi
}

# Main function
main() {
    check_commands rm
    parse_arguments "$@"
}

# Execute main function
main "$@"

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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./clear_chromium_cache.sh -c
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
#  Version History:
#  v1.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
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

# Clear Chromium "Web Data" directory
clear_cache() {
    cache_dir="$HOME/.config/chromium/Default/Web Data"

    if [ -e "$cache_dir" ]; then
        rm -rf "$cache_dir"
        echo "[INFO] Cleared: $cache_dir"
        exit 0
    else
        echo "[ERROR] Not found: $cache_dir" >&2
        exit 1
    fi
}

# Parse command-line arguments
parse_arguments() {
    while getopts "hc" opt; do
        case $opt in
            c)
                clear_cache
                ;;
            *)
                usage
                ;;
        esac
    done

    # If no options are provided, display help
    if [ $OPTIND -eq 1 ]; then
        usage
        exit 0
    fi
}

# Main entry point of the script
main() {
    parse_arguments "$@"
    return 0
}

# Execute main function
main "$@"
exit $?

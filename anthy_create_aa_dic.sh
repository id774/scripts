#!/bin/sh

########################################################################
# anthy_create_aa_dic.sh: Create Anthy Dictionary for ASCII Art
#
#  Description:
#  This script creates a custom dictionary for Anthy, a Japanese input
#  method, using ASCII art (AA) entries. It converts an MS-IME format
#  dictionary to the Canna format using the convert_msime2cannna.rb script.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.4 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.3 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.2 2025-03-17
#       Encapsulated all logic in functions and introduced main function.
#  v1.1 2023-12-06
#       Refactored for improved readability and added detailed comments.
#  v1.0 2009-11-17
#       Initial release.
#
#  Usage:
#      ./anthy_create_aa_dic.sh
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

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to this script." >&2
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

# Function to check for required files and directories
check_files() {
    test -f "$SCRIPTS/convert_msime2cannna.rb" || { echo "[ERROR] Missing convert_msime2cannna.rb" >&2; exit 1; }
    test -f "$SCRIPTS/etc/aa.txt" || { echo "[ERROR] Missing aa.txt" >&2; exit 1; }
    test -d "$HOME/.anthy" || mkdir -p "$HOME/.anthy"
}

# Function to create Anthy dictionary for ASCII Art
create_dictionary() {
    ruby -Ku "$SCRIPTS/convert_msime2cannna.rb" < "$SCRIPTS/etc/aa.txt" > "$HOME/.anthy/private_words_default"
    echo "[INFO] Dictionary created."
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac
    check_system
    check_scripts
    check_commands ruby mkdir
    check_files
    create_dictionary
}

# Execute main function
main "$@"

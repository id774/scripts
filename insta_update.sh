#!/bin/sh

########################################################################
# insta_update.sh: Instagram Content Update Script for Subdirectories
#
#  Description:
#  This script navigates to a specified directory and executes an Instagram
#  content downloader script in each subdirectory, followed by a synchronization
#  script. It supports optional reset and no-sync features, and allows for
#  specific account updates via the --account option. The reset feature
#  clears each target directory before downloading new content, safely renaming
#  the directory to ensure data integrity if the download fails. The no-sync
#  feature skips the synchronization step, allowing for faster updates when
#  synchronization is not needed. Configuration is managed through a file named
#  'insta_update.conf', specifying necessary settings. The script is intended
#  for POSIX-compliant shell environments.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.2 2025-03-17
#       Encapsulated all logic into functions and introduced main function.
#  v2.1 2025-01-02
#       Improved the "Running:" message to display the full absolute path
#       of the target subdirectory for better clarity.
#  v2.0 2024-10-23
#       Improved removal of trailing slashes and backslashes for
#       the --account option in insta_update.sh.
#       Added 'sed' to the list of required commands in the check_commands function
#       to ensure the script checks for its availability before execution.
#  v1.9 2024-08-19
#       Fixed issue where --account option was not working when include_accounts.txt was present.
#       Added logic to prioritize --account option when specified.
#  v1.8 2024-07-22
#       Added feature to ignore lines starting with '#'
#       in include_accounts.txt and exclude_accounts.txt as comments.
#  v1.7 2024-07-17
#       Updated processing order to follow include_accounts.txt as listed.
#       Ensured consistent variable scoping throughout the script.
#  v1.6 2024-06-15
#       Added --help option to display help message.
#  v1.5 2024-05-04
#       Handle trailing slashes in --account option by removing them.
#  v1.4 2024-04-25
#       Added --account option to specify a single account for updating.
#  v1.3 2024-03-21
#       Introduced the --no-sync (-n) option to skip the synchronization step.
#  v1.2 2024-02-23
#       Added check_commands function to verify the presence and executability
#       of required system commands before proceeding with the main script.
#  v1.1 2024-02-21
#       Updated the reset functionality to rename the target directory before
#       clearing, ensuring data is not lost if the download fails.
#  v1.0 2024-02-19
#       Initial release.
#
#  Usage:
#  ./insta_update.sh [--reset] [--no-sync] [--account ACCOUNT_NAME]
#  The --reset option safely renames the target directory before downloading new
#  content. The original directory is only removed if the download succeeds,
#  ensuring data integrity. The --no-sync (-n) option skips the synchronization
#  step, useful for faster updates when synchronization is not needed. The
#  --account option allows for updating a specific account only.
#
#  Configuration file ('insta_update.conf') requirements:
#  - PYTHON_BIN: Path to the Python binary.
#  - DOWNLOADER_SCRIPT: Path to the Instagram downloader script.
#  - SYNC_SCRIPT: Path to the Instagram synchronization script (optional with --no-sync).
#  - TARGET_DIR: Directory containing Instagram subdirectories.
#  Ensure all these variables are set in 'insta_update.conf'.
#
#  Notes:
#  - Ensure 'exclude_accounts.txt' and 'include_accounts.txt' are properly
#    formatted, with one account name per line, if they are used.
#  - The script supports 'exclude_accounts.txt' and 'include_accounts.txt' lists.
#    If 'exclude_accounts.txt' is present, any subdirectories matching the names
#    in this list will be skipped. If 'include_accounts.txt' is present, only the
#    subdirectories listed will be processed. If a subdirectory is listed in both,
#    it will be excluded.
#  - Lines starting with '#' in 'exclude_accounts.txt'
#    and 'include_accounts.txt' are treated as comments and ignored.
#
#  Error Conditions:
#  1. Configuration file not found or incomplete: The script will terminate if
#     it cannot find 'insta_update.conf' or if any required variables are unset.
#  2. Unknown command-line options: If any unrecognized options are provided,
#     the script will display an error message and exit.
#  3. Necessary configuration variables not set: The script checks if all
#     necessary configuration variables are set in 'insta_update.conf'.
#  4. Specified scripts or target directory do not exist or are not executable.
#  126. Required command(s) not executable.
#  127. Required command(s) not installed.
#
########################################################################

show_help() {
    cat << EOF
Usage: ${0##*/} [--reset] [--no-sync] [--account ACCOUNT_NAME] [--help]
This script navigates to a specified directory and executes an Instagram
content downloader script in each subdirectory, followed by a synchronization
script.

Options:
  --reset          Safely renames the target directory before downloading new content.
                   The original directory is only removed if the download succeeds,
                   ensuring data integrity.
  --no-sync, -n    Skips the synchronization step, useful for faster updates when synchronization is not needed.
  --account, -a    ACCOUNT_NAME
                   Allows for updating a specific account only.
  --help, -h       Display this help and exit.

Configuration file ('insta_update.conf') requirements:
  - PYTHON_BIN: Path to the Python binary.
  - DOWNLOADER_SCRIPT: Path to the Instagram downloader script.
  - SYNC_SCRIPT: Path to the Instagram synchronization script (optional with --no-sync).
  - TARGET_DIR: Directory containing Instagram subdirectories.
Ensure all these variables are set in 'insta_update.conf'.

Notes:
  - Ensure 'exclude_accounts.txt' and 'include_accounts.txt' are properly
    formatted, with one account name per line, if they are used.
  - The script supports 'exclude_accounts.txt' and 'include_accounts.txt' lists.
    If 'exclude_accounts.txt' is present, any subdirectories matching the names
    in this list will be skipped. If 'include_accounts.txt' is present, only the
    subdirectories listed will be processed. If a subdirectory is listed in both,
    it will be excluded.
  - Lines starting with '#' in 'exclude_accounts.txt'
    and 'include_accounts.txt' are treated as comments and ignored.

Error Conditions:
  1. Configuration file not found or incomplete: The script will terminate if
     it cannot find 'insta_update.conf' or if any required variables are unset.
  2. Unknown command-line options: If any unrecognized options are provided,
     the script will display an error message and exit.
  3. Necessary configuration variables not set: The script checks if all
     necessary configuration variables are set in 'insta_update.conf'.
  4. Specified scripts or target directory do not exist or are not executable.
  126. Required command(s) not executable.
  127. Required command(s) not installed.
EOF
}

load_and_validate_config() {
    # Determine the script's directory
    SCRIPT_DIR=$(dirname "$0")

    # Load configuration from a .conf file
    CONF_FILE="$SCRIPT_DIR/etc/insta_update.conf"
    if [ ! -f "$CONF_FILE" ]; then
        CONF_FILE="$SCRIPT_DIR/../etc/insta_update.conf"
        if [ ! -f "$CONF_FILE" ]; then
            echo "Error: Configuration file not found." >&2
            exit 1
        fi
    fi
    . "$CONF_FILE"

    # Check if necessary variables are set
    if [ -z "$PYTHON_BIN" ] || [ -z "$DOWNLOADER_SCRIPT" ] || [ -z "$SYNC_SCRIPT" ] || [ -z "$TARGET_DIR" ]; then
        echo "Error: One or more configuration variables are not set. Please check your insta_update.conf." >&2
        exit 3
    fi

    # Check if specified scripts and target directory exist and are executable
    if [ ! -x "$PYTHON_BIN" ] || [ ! -f "$DOWNLOADER_SCRIPT" ] || [ ! -x "$DOWNLOADER_SCRIPT" ] || [ ! -f "$SYNC_SCRIPT" ] || [ ! -x "$SYNC_SCRIPT" ] || [ ! -d "$TARGET_DIR" ]; then
        echo "Error: Specified scripts or target directory do not exist or are not executable." >&2
        exit 4
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

# Define a function to trim and ignore comments
trim_and_ignore_comments() {
    echo "$1" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//' | grep -v '^#'
}

parse_options() {
    RESET=false
    NO_SYNC=false
    ACCOUNT_SPECIFIED=""
    EXCLUDE_LIST="$TARGET_DIR/exclude_accounts.txt"
    INCLUDE_LIST="$TARGET_DIR/include_accounts.txt"

    # Parse options
    while [ $# -gt 0 ]; do
        case $1 in
            --reset)
                RESET=true
                shift
                ;;
            --no-sync|-n)
                NO_SYNC=true
                shift
                ;;
            --account|-a)
                if [ -n "$2" ]; then
                    # Remove trailing slash, if any
                    ACCOUNT_SPECIFIED=$(echo "$2" | sed 's:[/\\]*$::')
                    shift 2
                else
                    echo "Error: --account option requires a value." >&2
                    exit 2
                fi
                ;;
            --help|-h)
                show_help
                exit 0
                ;;
            *)
                # Unknown option
                echo "Error: Unknown option $1" >&2
                exit 2
                ;;
        esac
    done
}

update_content() {
    subdir="$1"

    if [ "$RESET" = true ]; then
        echo "Resetting directory: $subdir"
        subdir_basename=$(basename "$subdir")  # Extract the directory name
        old_dir="${TARGET_DIR}/${subdir_basename}_old"

        if [ -d "$old_dir" ]; then
            echo "Removing existing backup directory: ${subdir_basename}_old"
            rm -rf "$old_dir"  # Remove existing backup directory if it exists
        fi

        mv "$subdir" "$old_dir"  # Rename the directory to *_old
        mkdir -p "$subdir"  # Create a new directory with the original name
    fi

    echo "Running: $PYTHON_BIN $DOWNLOADER_SCRIPT in $(cd "$subdir" && pwd)"
    cd "$subdir" || exit
    "$PYTHON_BIN" "$DOWNLOADER_SCRIPT" || exit
    cd "$TARGET_DIR" || exit
    if [ "$NO_SYNC" = false ]; then
        echo "Synchronizing: $SYNC_SCRIPT $(basename "$subdir")"
        "$SYNC_SCRIPT" "$(basename "$subdir")" || exit
    fi
}

should_process() {
    subdir_name=$(basename "$1")

    # If an account is specified in the command line, process only that account
    if [ -n "$ACCOUNT_SPECIFIED" ]; then
        if [ "$subdir_name" = "$ACCOUNT_SPECIFIED" ]; then
            return 0  # Process this directory
        else
            return 1  # Skip this directory
        fi
    fi

    # Skip if subdir is in exclude list
    if [ -f "$EXCLUDE_LIST" ]; then
        while IFS= read -r line; do
            line=$(trim_and_ignore_comments "$line")
            if [ -n "$line" ] && [ "$line" = "$subdir_name" ]; then
                return 1
            fi
        done < "$EXCLUDE_LIST"
    fi

    # Process if subdir is in include list
    if [ -f "$INCLUDE_LIST" ]; then
        while IFS= read -r line; do
            line=$(trim_and_ignore_comments "$line")
            if [ -n "$line" ] && [ "$line" = "$subdir_name" ]; then
                return 0
            fi
        done < "$INCLUDE_LIST"
        return 1  # Skip if subdir is not in include list
    fi

    return 0  # Default to process if no include list is provided and no account is specified
}

# Function to change to the target directory
change_to_target_dir() {
    cd "$TARGET_DIR" || exit
}

# Main function to execute the script
main() {
    load_and_validate_config

    # Ensure necessary commands are available
    check_commands mv mkdir rm grep sed

    parse_options "$@"
    change_to_target_dir

    # Check if include_accounts.txt exists and process in the order listed
    if [ -n "$ACCOUNT_SPECIFIED" ]; then
        subdir="${ACCOUNT_SPECIFIED}/"
        if [ -d "$subdir" ] && should_process "$subdir"; then
            update_content "$subdir"
        fi
    elif [ -f "$INCLUDE_LIST" ]; then
        while IFS= read -r account; do
            account=$(trim_and_ignore_comments "$account")
            if [ -n "$account" ]; then
                subdir="${account}/"
                if [ -d "$subdir" ] && should_process "$subdir"; then
                    update_content "$subdir"
                fi
            fi
        done < "$INCLUDE_LIST"
    else
        for subdir in */ ; do
            if [ -d "$subdir" ] && should_process "$subdir"; then
                update_content "$subdir"
            fi
        done
    fi
}

# Execute main function
main "$@"

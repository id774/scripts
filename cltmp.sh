#!/bin/sh

########################################################################
# cltmp.sh: Cleanup Temporary and Cached Files
#
#  Description:
#  This script is designed to clean up temporary and cached files on Unix-like systems.
#  It includes specific operations for both macOS (Darwin) and Linux platforms, such as
#  removing old files from designated directories and clearing system caches.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  20250413 - Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  20250322 - Unify usage information by extracting help text from header comments.
#  20250317 - Encapsulated all logic in functions and introduced main function.
#  20250313 - Redirected error messages to stderr for better logging and debugging.
#  20250304 - Moved .netrwhist cleanup to cltmp.sh.
#  20250303 - Added removal of unnecessary files in the home directory.
#  20250119 - Fix wildcard handling in rm commands to ensure proper file deletion.
#  20241212 - Refined hardcopy cleanup to target numbered files explicitly.
#  20241210 - Enhanced wget-log cleanup to include related files.
#  20241204 - Refactored cleanup logic using clean_dir() function for better maintainability.
#  20230827 - Latest update with specific cleanup operations for macOS and Linux.
#
#  Usage:
#  Run the script without any arguments:
#      ./cltmp.sh
#
#  The script automatically detects the operating system and performs the appropriate
#  cleanup actions. Ensure to have the necessary permissions before running.
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

# Function to clean a directory with specified conditions
clean_dir() {
    dir=$1
    days=$2
    cmd=${3:-"rm -vf"}
    if [ -d "$dir" ]; then
        find "$dir" -type f -mtime "+$days" -exec $cmd {} \;
    fi
}

# Perform cleanup based on OS
perform_cleanup() {
    os=$(uname)

    if [ "$os" = "Darwin" ]; then
        test -L "$HOME/Desktop/場所が変更された項目" && rm -f "$HOME/Desktop/場所が変更された項目"
        for dir in "$HOME/Pictures" "$HOME/Documents" "$HOME/Downloads" "$HOME/Desktop"; do
            test -d "$dir" && touch "$dir/.localized"
        done
        clean_dir "$HOME/tmp" 3 "rm -vf"
    elif [ "$os" = "Linux" ]; then
        if [ "$(id -u)" -eq 0 ]; then
            clean_dir /root/.cache 0 "rm -vrf"
        fi
        rm -vf "$HOME/hardcopy."[0-9]*
        clean_dir "$HOME/tmp" 1 "rm -vf"
    fi

    rm -vf "$HOME/wget-log" "$HOME"/wget-log.[0-9]*

    for dir in "$HOME/.gem" "$HOME/.pip" "$HOME/.npm"; do
        clean_dir "$dir" 0 "rm -vrf"
    done

    for dir in "$HOME/.tmp" "$HOME/twitter_viewer/log" "$HOME/fastladder/log"; do
        clean_dir "$dir" 7 "rm -vf"
    done

    for dir in "$HOME/.emacs.d/tmp" "$HOME/.emacs.d/backups" "$HOME/.emacs.d/auto-save-list" "$HOME/.emacs.d/tramp-auto-save"; do
        clean_dir "$dir" 30 "rm -vf"
    done

    if [ "$os" = "Darwin" ]; then
        if [ "$(id -u)" -eq 0 ] || ! type trash >/dev/null 2>&1; then
            # Use rm for root or if trash is unavailable
            for dir in "$HOME/Pictures" "$HOME/Documents"; do
                clean_dir "$dir" 30 "rm -vf"
            done
            for dir in "$HOME/Downloads" "$HOME/Desktop"; do
                clean_dir "$dir" 7 "rm -vf"
            done
        else
            # Use trash for non-root users
            trash -ev
            for dir in "$HOME/Pictures" "$HOME/Documents"; do
                clean_dir "$dir" 7 trash
            done
            for dir in "$HOME/Downloads" "$HOME/Desktop"; do
                clean_dir "$dir" 3 trash
            done
            echo "Show trash contents..."
            trash -lv
        fi
    else
        for dir in "$HOME/Pictures" "$HOME/Documents"; do
            clean_dir "$dir" 30 "rm -vf"
        done
        for dir in "$HOME/Downloads" "$HOME/Desktop"; do
            clean_dir "$dir" 7 "rm -vf"
        done
    fi

    # Additional cleanup
    rm -vf "$HOME/.bash_history"
    rm -vf "$HOME/.recentf~"
    rm -vf "$HOME/.xsession-errors"
    rm -vrf "$HOME/.cache/*"
    rm -vrf "$HOME/.local/share/Trash/*"
    rm -vf "$HOME"/.vim/.netrwhist
    rm -vf "$HOME"/.emacs.d/*~
    rm -vf "$HOME"/*.swp "$HOME"/*.swo "$HOME"/*.bak "$HOME"/*.~ "$HOME"/*.old
    rm -vf "$HOME"/.*.swp "$HOME"/.*.swo "$HOME"/.*.bak "$HOME"/.*.~ "$HOME"/.*.old

    echo "[INFO] cltmp (20250413) done."
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_commands find rm uname
    perform_cleanup
}

# Execute main function
main "$@"

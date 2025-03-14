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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
#      cltmp.sh
#
#  The script automatically detects the operating system and performs the appropriate
#  cleanup actions. Ensure to have the necessary permissions before running.
#
########################################################################

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

# Check required commands
check_commands find rm uname

os=$(uname)

# Function to clean a directory with specified conditions
clean_dir() {
    dir=$1
    days=$2
    cmd=${3:-"rm -vf"}
    if [ -d "$dir" ]; then
        find "$dir" -type f -mtime "+$days" -exec $cmd {} \;
    fi
}

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
    rm -vf $HOME/hardcopy.[0-9]*
    clean_dir "$HOME/tmp" 1 "rm -vf"
fi

rm -vf "$HOME/wget-log" $HOME/wget-log.[0-9]*

for dir in "$HOME/.gem" "$HOME/.pip" "$HOME/.npm"; do
    clean_dir "$dir" 0 "rm -vrf"
done

# Common clean-up for 7 days
for dir in "$HOME/.tmp" "$HOME/twitter_viewer/log" "$HOME/fastladder/log"; do
    clean_dir "$dir" 7 "rm -vf"
done

# Common clean-up for 30 days
for dir in "$HOME/.emacs.d/tmp" "$HOME/.emacs.d/backups" "$HOME/.emacs.d/auto-save-list" "$HOME/.emacs.d/tramp-auto-save"; do
    clean_dir "$dir" 30 "rm -vf"
done

if [ "$os" = "Darwin" ]; then
    if [ "$(id -u)" -eq 0 ] || ! type trash &> /dev/null; then
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
rm -vf "$HOME"/*.swp "$HOME"/*.swo "$HOME"/*.bak "$HOME"/*.~ "$HOME"/*.old
rm -vf "$HOME"/.*.swp "$HOME"/.*.swo "$HOME"/.*.bak "$HOME"/.*.~ "$HOME"/.*.old

echo "cltmp (20250313) done."

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
#  20241204 - Fixed incorrect directory reference in cleanup logic for $HOME/Documents.
#             Refactored cleanup logic using clean_dir() function for better maintainability.
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

os=$(uname)

# Function to clean a directory with specified conditions
clean_dir() {
    dir=$1
    days=$2
    cmd=${3:-rm}
    if [ -d "$dir" ]; then
        find "$dir" -type f -mtime "+$days" -exec $cmd -vf {} \;
    fi
}

if [ "$os" = "Darwin" ]; then
    test -L "$HOME/Desktop/場所が変更された項目" && rm -f "$HOME/Desktop/場所が変更された項目"
    for dir in "$HOME/Pictures" "$HOME/Documents" "$HOME/Downloads" "$HOME/Desktop"; do
        test -d "$dir" && touch "$dir/.localized"
    done
    clean_dir "$HOME/tmp" 3
elif [ "$os" = "Linux" ]; then
    rm -vrf /root/.cache
    rm -vf "$HOME/hardcopy.*"
    clean_dir "$HOME/tmp" 1
fi

rm -vf "$HOME/wget-log*"
rm -vf "$HOME/.emacs.d/%backup%$HOME" "$HOME/%backup%$HOME"
for dir in "$HOME/.gem" "$HOME/.pip" "$HOME/.npm"; do
    rm -vrf "$dir"
done

clean_dir "$HOME/.tmp" 7
clean_dir "$HOME/.emacs.d/tmp" 30
clean_dir "$HOME/.emacs.d/backups" 30
clean_dir "$HOME/.emacs.d/auto-save-list" 30
clean_dir "$HOME/.emacs.d/tramp-auto-save" 30
clean_dir "$HOME/twitter_viewer/log" 7
clean_dir "$HOME/fastladder/log" 7

if [ "$os" = "Darwin" ]; then
    if type trash &> /dev/null; then
        trash -ev
        clean_dir "$HOME/Pictures" 7 trash
        clean_dir "$HOME/Documents" 7 trash
        clean_dir "$HOME/Downloads" 3 trash
        clean_dir "$HOME/Desktop" 3 trash
        echo "Show trash contents..."
        trash -lv
    else
        clean_dir "$HOME/Pictures" 30
        clean_dir "$HOME/Documents" 30
        clean_dir "$HOME/Downloads" 7
        clean_dir "$HOME/Desktop" 7
    fi
else
    clean_dir "$HOME/Pictures" 30
    clean_dir "$HOME/Documents" 30
    clean_dir "$HOME/Downloads" 7
    clean_dir "$HOME/Desktop" 7
fi

echo "cltmp (20241204) done."

#!/bin/sh

########################################################################
# uninstall_mecab_local.sh: Uninstall MeCab, CaboCha, CRF++ and neologd from /usr/local
#
#  Description:
#  This script removes NLP components installed via source into /usr/local,
#  including MeCab, mecab-ipadic-neologd, CaboCha, and CRF++.
#  It scans and deletes known binaries, libraries, headers, configuration files,
#  and dictionaries placed in /usr/local by custom installation scripts.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./uninstall_mecab_local.sh          # Show help message only
#      ./uninstall_mecab_local.sh --delete # Actually perform uninstallation
#
#  Features:
#  - Displays step-by-step removal messages for transparency.
#  - Only performs deletion when explicitly passed "--delete".
#  - Skips non-existent files safely with explanatory output.
#
#  Warning:
#  - This script removes files and directories from /usr/local without confirmation.
#  - Only use this if you understand the implications of removing manually installed packages.
#  - It does not uninstall packages installed via package manager (e.g. apt/yum).
#
#  Version History:
#  v1.2 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.1 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.0 2025-03-27
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
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Function: remove file or directory with explanation
remove_item() {
    path=$1
    if [ -e "$path" ] || [ -L "$path" ]; then
        echo "[INFO] Removing: $path"
        if ! sudo rm -rf "$path"; then
            echo "[ERROR] Failed to remove: $path" >&2
            exit 1
        fi
        echo "[INFO] Successfully removed: $path"
    else
        echo "[INFO] Skipping (not found): $path"
    fi
    echo
}

# Uninstall MeCab related files from /usr/local
uninstall_mecab() {
    echo "[INFO] Removing Mecab..."
    remove_item /usr/local/bin/mecab
    remove_item /usr/local/bin/mecab-config
    remove_item /usr/local/include/mecab.h
    remove_item /usr/local/lib/libmecab.so
    remove_item /usr/local/lib/libmecab.so.2
    remove_item /usr/local/lib/libmecab.so.2.0.0
    remove_item /usr/local/lib/libmecab.la
    remove_item /usr/local/lib/libmecab.a
    remove_item /usr/local/lib/pkgconfig/mecab.pc
    remove_item /usr/local/share/mecab
    remove_item /usr/local/share/man/man1/mecab.1
    remove_item /usr/local/etc/mecabrc
    remove_item /usr/local/lib/mecab
    remove_item /usr/local/libexec/mecab
}

# Uninstall CaboCha related files from /usr/local
uninstall_cabocha() {
    echo "[INFO] Removing Cabocha..."
    remove_item /usr/local/bin/cabocha
    remove_item /usr/local/bin/cabocha-config
    remove_item /usr/local/include/cabocha.h
    remove_item /usr/local/lib/libcabocha.so
    remove_item /usr/local/lib/libcabocha.so.4
    remove_item /usr/local/lib/libcabocha.so.4.0.0
    remove_item /usr/local/lib/libcabocha.la
    remove_item /usr/local/lib/libcabocha.a
    remove_item /usr/local/lib/pkgconfig/cabocha.pc
    remove_item /usr/local/share/cabocha
    remove_item /usr/local/share/man/man1/cabocha.1
    remove_item /usr/local/etc/cabocharc
    remove_item /usr/local/lib/cabocha
    remove_item /usr/local/libexec/cabocha
}

# Uninstall CRF++ related files from /usr/local
uninstall_crfpp() {
    echo "[INFO] Removing CRF++..."
    remove_item /usr/local/bin/crf_learn
    remove_item /usr/local/bin/crf_test
    remove_item /usr/local/include/crfpp.h
    remove_item /usr/local/lib/libcrfpp.so
    remove_item /usr/local/lib/libcrfpp.so.0
    remove_item /usr/local/lib/libcrfpp.so.0.0.0
    remove_item /usr/local/lib/libcrfpp.la
    remove_item /usr/local/lib/libcrfpp.a
    remove_item /usr/local/lib/pkgconfig/crfpp.pc
    remove_item /usr/local/share/crfpp
}

# Uninstall mecab-ipadic-neologd dictionary
uninstall_neologd() {
    echo "[INFO] Removing NEologd..."
    remove_item /usr/local/lib/mecab/dic/mecab-ipadic-neologd
}

# Main entry point of the script
main() {
    if [ "$#" -ne 1 ] || [ "$1" != "--delete" ]; then
        usage
    fi

    check_system
    check_commands rm
    check_sudo

    echo "[INFO] Starting uninstallation process from /usr/local."
    echo

    uninstall_mecab
    uninstall_cabocha
    uninstall_crfpp
    uninstall_neologd

    echo "[INFO] Uninstallation process completed."
    return 0
}

# Execute main function
main "$@"
exit $?

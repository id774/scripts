#!/bin/sh

########################################################################
# install_brews.sh: Bulk Homebrew Library Install Script
#
#  Description:
#  This script installs a set of essential tools and libraries on macOS
#  using Homebrew. It ensures that required software is installed and
#  configured for development, text processing, system administration,
#  and other tasks.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script in a terminal to set up your macOS environment.
#  Examples:
#     ./install_brews.sh
#
#  Requirements:
#  - macOS with Homebrew installed and available in PATH.
#
#  Exit Status:
#  0: Success - The batch workflow completed.
#  1: Error - macOS validation or one or more Homebrew setup operations failed.
#  126: Error - A required command exists but is not executable.
#  127: Error - A required command is not found.
#
#  Notes:
#  - This script ensures the use of GNU Coreutils on macOS for consistent
#    behavior across platforms.
#  - Force-links OpenSSL to ensure compatibility with applications requiring
#    the latest version.
#  - `trash` is installed for safer file deletions, replacing `rm`.
#
#  Version History:
#  v1.9 2026-09-20
#       Use the shared Homebrew prerequisite contract and report batch
#       operation failures without stopping independent install attempts.
#  v1.8 2026-09-07
#       Check uname before system detection and clarify batch completion.
#  v1.7 2026-07-11
#       Replace the awk {n,} interval expression in usage() with a portable
#       equivalent, since mawk on some systems matches it incorrectly.
#  v1.6 2026-02-05
#       Remove preflight network connectivity check.
#  v1.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.4 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.3 2025-03-27
#       Added explicit 'brew cleanup' step to remove old versions and caches.
#  v1.2 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.1 2025-03-16
#       Added macOS system check and network connectivity validation.
#  v1.0 2025-01-16
#       Initial stable release. Added support for installing essential tools like
#       OpenSSL, Coreutils, Vim, and MeCab via Homebrew.
#  v0.1 2016-04-18
#       Initial release.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    check_commands awk
    awk '
        BEGIN { in_header = 0 }
        /^#+$/ && length($0) >= 10 { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the system is macOS
check_system() {
    check_commands uname
    if [ "$(uname)" != "Darwin" ]; then
        echo "[ERROR] This script is intended for macOS only." >&2
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

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands brew

    status=0

    # Check Homebrew environment
    echo "[INFO] Running 'brew doctor' to check the system's Homebrew environment..."
    brew doctor || status=1

    # Update Homebrew
    echo "[INFO] Updating Homebrew packages..."
    brew update || status=1

    # Install essential tools and libraries
    echo "[INFO] Installing essential tools and libraries using Homebrew..."
    brew install openssl || status=1
    brew link openssl --force || status=1
    brew install wget || status=1
    brew install nkf || status=1
    brew install vim || status=1
    brew install nvim || status=1
    brew install freetype || status=1
    brew install rsync || status=1
    brew install smartmontools || status=1
    brew install mecab || status=1
    brew install cabocha || status=1
    brew install ta-lib || status=1
    brew install trash || status=1
    brew install coreutils || status=1
    brew install findutils || status=1
    brew install moreutils || status=1
    brew install binutils || status=1

    # Cleanup old versions and caches to free up disk space
    echo "[INFO] Cleaning up old versions and caches..."
    brew cleanup || status=1

    if [ "$status" -eq 0 ]; then
        echo "[INFO] All specified brew packages have been installed."
    else
        echo "[ERROR] One or more Homebrew setup operations failed." >&2
    fi

    return $status
}

# Execute main function
main "$@"

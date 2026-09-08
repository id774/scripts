#!/bin/sh

########################################################################
# reinstall_brew.sh: Reinstall Homebrew on macOS
#
#  Description:
#  This script completely removes and reinstalls Homebrew on macOS by:
#  - Determining the official Homebrew default layout from the current
#    process architecture.
#  - Running the official uninstaller non-interactively against the
#    architecture-default prefix.
#  - Removing the residual Cellar and repository of that layout.
#  - Installing Homebrew from the official source.
#  - Loading the newly installed Homebrew environment into the current
#    process.
#  - Running install_brews.sh for required packages.
#  - Running fix_compinit.sh to fix compinit issues.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script to reinstall Homebrew:
#      ./reinstall_brew.sh
#
#  Requirements:
#  - Must be executed on macOS with an arm64 or x86_64 process architecture.
#  - Run as a regular non-root user with sudo privileges.
#
#  Notes:
#  - Existing Homebrew does not need to be executable or available in PATH
#    before reinstall.
#  - The target follows the current process architecture, matching the
#    official Homebrew installer: /opt/homebrew for arm64 and /usr/local
#    for x86_64.
#  - The script loads `brew shellenv sh` only into the current process after
#    reinstall and does not edit the user's shell startup files.
#
#  Exit Status:
#  0: Success.
#  1: Unsupported environment, root execution, sudo failure,
#     reinstall setup failure, or post-install Homebrew environment failure.
#  126: Required command exists but is not executable.
#  127: Required command is not found.
#
#  Version History:
#  v1.6 2026-09-07
#       Reinstall Homebrew by process architecture and load its new environment.
#  v1.5 2026-07-11
#       Replace the awk {n,} interval expression in usage() with a portable
#       equivalent, since mawk on some systems matches it incorrectly.
#  v1.4 2026-02-05
#       Remove preflight network connectivity check.
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-16
#       Initial stable release with full reinstall process.
#  v0.1 2022-09-22
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

# Check if required commands exist
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

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges." >&2
        exit 1
    fi
}

# Check if required scripts exist
check_required_scripts() {
    for script in "$SCRIPTS/installer/install_brews.sh" "$SCRIPTS/fix_compinit.sh"; do
        if [ ! -f "$script" ]; then
            echo "[ERROR] Required script '$script' not found." >&2
            exit 1
        fi
    done
}

# Refuse whole-script root execution
check_user() {
    current_uid=$(id -u 2>/dev/null)
    if [ $? -ne 0 ] || [ -z "$current_uid" ]; then
        echo "[ERROR] Failed to determine the invoking user." >&2
        exit 1
    fi
    if [ "$current_uid" -eq 0 ]; then
        echo "[ERROR] Run this script as a regular user with sudo access, not as root." >&2
        exit 1
    fi
    return 0
}

# Determine the official Homebrew default layout from the process architecture
resolve_homebrew_layout() {
    arch=$(uname -m 2>/dev/null)
    case "$arch" in
        arm64)
            homebrew_prefix=/opt/homebrew
            homebrew_repository=/opt/homebrew
            homebrew_cellar=/opt/homebrew/Cellar
            homebrew_brew=/opt/homebrew/bin/brew
            ;;
        x86_64)
            homebrew_prefix=/usr/local
            homebrew_repository=/usr/local/Homebrew
            homebrew_cellar=/usr/local/Cellar
            homebrew_brew=/usr/local/bin/brew
            ;;
        *)
            echo "[ERROR] Unsupported macOS architecture: $arch" >&2
            exit 1
            ;;
    esac
}

# Load the newly installed Homebrew environment into the current process
load_homebrew_environment() {
    if [ ! -x "$homebrew_brew" ]; then
        echo "[ERROR] Newly installed Homebrew is not executable at '$homebrew_brew'." >&2
        exit 1
    fi

    shellenv=$("$homebrew_brew" shellenv sh)
    if [ $? -ne 0 ]; then
        echo "[ERROR] Failed to load the newly installed Homebrew environment." >&2
        exit 1
    fi
    if [ -n "$shellenv" ]; then
        if ! eval "$shellenv"; then
            echo "[ERROR] Failed to load the newly installed Homebrew environment." >&2
            exit 1
        fi
    fi

    if ! command -v brew >/dev/null 2>&1; then
        echo "[ERROR] Newly installed Homebrew is not available in PATH." >&2
        exit 1
    fi

    actual_prefix=$(brew --prefix 2>/dev/null)
    if [ $? -ne 0 ] || [ "$actual_prefix" != "$homebrew_prefix" ]; then
        echo "[ERROR] Newly installed Homebrew prefix does not match '$homebrew_prefix'." >&2
        exit 1
    fi
}

# Reinstall Homebrew
reinstall_homebrew() {
    echo "[INFO] Reinstalling Homebrew..."

    # Uninstall Homebrew non-interactively from the architecture-default prefix
    NONINTERACTIVE=1 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/uninstall.sh)" uninstall.sh --path="$homebrew_prefix"

    # Remove residual directories of the same layout
    sudo rm -rf "$homebrew_cellar" "$homebrew_repository"

    # Install Homebrew
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

    # Make the new Homebrew available to this process and its children
    load_homebrew_environment

    # Run batch installation script for required packages
    "$SCRIPTS/installer/install_brews.sh"

    # Fix compinit issues
    "$SCRIPTS/fix_compinit.sh"

    echo "[INFO] Homebrew reinstallation completed."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_system
    check_commands curl rm bash id
    check_scripts
    check_required_scripts
    check_user
    resolve_homebrew_layout
    check_sudo
    reinstall_homebrew
    return 0
}

# Execute main function
main "$@"

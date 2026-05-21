#!/bin/sh

########################################################################
# git-archive-repo.sh: Git Repository Archive Script
#
#  Description:
#  This script creates compressed archive files from local Git repository
#  directories. By default, it archives ~/local/github and ~/local/git into
#  ~/user2/arc/git/github.tar.gz and ~/user2/arc/git/git.tar.gz.
#
#  The script can load an optional configuration file named
#  'git-archive-repo.conf' to override the default source and archive paths.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script without any arguments:
#      ./git-archive-repo.sh
#
#  Configuration file ('git-archive-repo.conf') variables:
#  - GITHUB_SRC     : Source directory for GitHub repositories.
#  - GIT_SRC        : Source directory for local Git repositories.
#  - GITHUB_ARCHIVE : Archive file path for GitHub repositories.
#  - GIT_ARCHIVE    : Archive file path for local Git repositories.
#
#  Default values:
#  - GITHUB_SRC="$HOME/local/github"
#  - GIT_SRC="$HOME/local/git"
#  - GITHUB_ARCHIVE="$HOME/user2/arc/git/github.tar.gz"
#  - GIT_ARCHIVE="$HOME/user2/arc/git/git.tar.gz"
#
#  Notes:
#  - Existing archive files are removed before archive creation.
#  - Archive parent directories are created automatically if missing.
#  - Each archive stores the source directory by its basename, not by its
#    absolute path.
#
#  Error Conditions:
#  1. Source directory not found.
#  2. Failed to create archive parent directory.
#  3. Failed to remove existing archive.
#  4. Failed to create archive.
#  126. Required command(s) not executable.
#  127. Required command(s) not installed.
#
#  Version History:
#  v1.0 2026-05-21
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

# Check for required commands
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

# Apply default configuration values
apply_defaults() {
    GITHUB_SRC=${GITHUB_SRC:-"$HOME/local/github"}
    GIT_SRC=${GIT_SRC:-"$HOME/local/git"}
    GITHUB_ARCHIVE=${GITHUB_ARCHIVE:-"$HOME/arc/github.tar.gz"}
    GIT_ARCHIVE=${GIT_ARCHIVE:-"$HOME/arc/git.tar.gz"}
}

# Load optional configuration
load_configuration() {
    CONF_FILE="$SCRIPT_DIR/etc/git-archive-repo.conf"
    if [ ! -f "$CONF_FILE" ]; then
        CONF_FILE="$SCRIPT_DIR/../etc/git-archive-repo.conf"
    fi

    if [ -f "$CONF_FILE" ]; then
        echo "[INFO] Loaded configuration from $CONF_FILE."
        . "$CONF_FILE"
    else
        echo "[INFO] Configuration file not found. Using default settings."
    fi

    apply_defaults
}

# Create archive from a source directory
create_archive() {
    source_dir="$1"
    archive_file="$2"

    if [ ! -d "$source_dir" ]; then
        echo "[ERROR] Source directory not found: $source_dir" >&2
        return 1
    fi

    archive_dir=$(dirname "$archive_file")
    source_parent=$(dirname "$source_dir")
    source_base=$(basename "$source_dir")

    if [ ! -d "$archive_dir" ]; then
        echo "[INFO] Creating archive directory: $archive_dir"
        mkdir -p "$archive_dir" || return 2
    fi

    if [ -f "$archive_file" ]; then
        echo "[INFO] Removing existing archive: $archive_file"
        rm -f "$archive_file" || return 3
    fi

    echo "[INFO] Creating archive: $archive_file from $source_dir"
    if tar czf "$archive_file" -C "$source_parent" "$source_base"; then
        echo "[INFO] Archive created successfully: $archive_file"
    else
        echo "[ERROR] Failed to create archive: $archive_file" >&2
        return 4
    fi

    return 0
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    SCRIPT_DIR=$(dirname "$0")

    check_commands awk basename dirname mkdir rm tar
    load_configuration

    create_archive "$GITHUB_SRC" "$GITHUB_ARCHIVE" || exit $?
    create_archive "$GIT_SRC" "$GIT_ARCHIVE" || exit $?

    echo "[INFO] All archives created successfully."
    return 0
}

# Execute main function
main "$@"

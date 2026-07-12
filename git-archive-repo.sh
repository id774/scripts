#!/bin/sh

########################################################################
# git-archive-repo.sh: Git Repository Archive Script
#
#  Description:
#  This script creates compressed archive files from local Git repository
#  directories, using source and archive paths defined in a required
#  configuration file.
#
#  The script loads a configuration file named 'git-archive-repo.conf',
#  which must define the source and archive paths described below.
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
#  Configuration file ('git-archive-repo.conf') variables (all required):
#  - GITHUB_SRC     : Source directory for GitHub repositories.
#  - GIT_SRC        : Source directory for local Git repositories.
#  - GITHUB_ARCHIVE : Archive file path for GitHub repositories.
#  - GIT_ARCHIVE    : Archive file path for local Git repositories.
#
#  Notes:
#  - Existing archive files are removed before archive creation.
#  - Archive parent directories must already exist; they are not created
#    automatically.
#  - Each archive stores the source directory by its basename, not by its
#    absolute path.
#
#  Error Conditions:
#  1. Source directory not found.
#  2. Archive parent directory not found.
#  3. Failed to remove existing archive.
#  4. Failed to create archive.
#  5. Configuration file not found or a required variable is not set.
#  126. Required command(s) not executable.
#  127. Required command(s) not installed.
#
#  Version History:
#  v1.2 2026-07-12
#       Require the configuration file and all its variables, and stop
#       creating missing source or archive directories, treating them as
#       errors instead.
#  v1.1 2026-07-11
#       Replace the awk {n,} interval expression in usage() with a portable
#       equivalent, since mawk on some systems matches it incorrectly.
#  v1.0 2026-05-21
#       Initial release.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#+$/ && length($0) >= 10 { if (!in_header) { in_header = 1; next } else exit }
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

# Verify that all required configuration variables are set
check_configuration() {
    for var in GITHUB_SRC GIT_SRC GITHUB_ARCHIVE GIT_ARCHIVE; do
        eval "value=\${$var:-}"
        if [ -z "$value" ]; then
            echo "[ERROR] Required configuration variable '$var' is not set." >&2
            exit 5
        fi
    done
}

# Load required configuration
load_configuration() {
    CONF_FILE="$SCRIPT_DIR/etc/git-archive-repo.conf"
    if [ ! -f "$CONF_FILE" ]; then
        CONF_FILE="$SCRIPT_DIR/../etc/git-archive-repo.conf"
    fi

    if [ ! -f "$CONF_FILE" ]; then
        echo "[ERROR] Configuration file not found: git-archive-repo.conf" >&2
        exit 5
    fi

    echo "[INFO] Loaded configuration from $CONF_FILE."
    . "$CONF_FILE"

    check_configuration
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
        echo "[ERROR] Archive directory not found: $archive_dir" >&2
        return 2
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

    check_commands awk basename dirname rm tar
    load_configuration

    create_archive "$GITHUB_SRC" "$GITHUB_ARCHIVE" || exit $?
    create_archive "$GIT_SRC" "$GIT_ARCHIVE" || exit $?

    echo "[INFO] All archives created successfully."
    return 0
}

# Execute main function
main "$@"

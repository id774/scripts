#!/bin/sh

########################################################################
# git-all-pull.sh: Git Repositories Pull Script
#
#  Description:
#  This script performs a 'git pull' on all Git repositories in specified
#  local directories. It also checks for and creates symbolic links from
#  the home directory to these repositories.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.3 2025-03-16
#       Refactored entire script to encapsulate all logic in functions.
#       Introduced `main()` function for better structure and maintainability.
#  v1.2 2024-01-07
#       Updated command existence and execution permission checks.
#  v1.1 2023-12-07
#       Added checks for Git installation and improved options handling.
#  v1.0 2023-12-05
#       Initial release.
#
#  Usage:
#      ./git-all-pull.sh [--hard] [--no-symlink] [--dry-run] [--github-only] [--git-only] [--all]
#
#  Default behavior is to show this help message. Use '--all' to pull from both github and git directories.
#
#  WARNING: The '--hard' option performs 'git reset --hard' which can
#  overwrite local changes. Use with caution.
#
########################################################################

# Global variables
HARD_MODE=false
NO_SYMLINK=false
DRY_RUN=false
GITHUB_ONLY=false
GIT_ONLY=false
ALL=false
SHOW_HELP=false

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

# Parse command-line arguments
parse_arguments() {
    if [ $# -eq 0 ]; then
        SHOW_HELP=true
    fi

    for arg in "$@"; do
        case "$arg" in
            --hard) HARD_MODE=true ;;
            --no-symlink) NO_SYMLINK=true ;;
            --dry-run) DRY_RUN=true ;;
            --github-only) GITHUB_ONLY=true ;;
            --git-only) GIT_ONLY=true ;;
            --all) ALL=true ;;
            *) SHOW_HELP=true ;;
        esac
    done

    if [ "$SHOW_HELP" = true ]; then
        usage
    fi
}

# Pull updates from a Git repository
pull_repo() {
    repo="$1"

    if [ "$HARD_MODE" = true ]; then
        if [ "$DRY_RUN" = false ]; then
            echo "[INFO] Resetting repository: $repo"
            git -C "$repo" clean -dxf
            git -C "$repo" reset --hard
        else
            echo "[INFO] DRY RUN: Reset repository: $repo"
        fi
    fi

    if [ "$DRY_RUN" = false ]; then
        echo "[INFO] Pulling repository: $repo"
        git -C "$repo" pull
    else
        echo "[INFO] DRY RUN: Pull repository: $repo"
    fi
}

# Create a symbolic link in the home directory for a repository
create_symlink() {
    repo="$1"
    link_path="$HOME/$(basename "$repo")"

    if [ ! -L "$link_path" ]; then
        if [ "$DRY_RUN" = false ]; then
            echo "[INFO] Creating symlink: $link_path -> $repo"
            ln -s "$repo" "$link_path"
        else
            echo "[INFO] DRY RUN: Create symlink: $link_path -> $repo"
        fi
    fi
}

# Process all repositories in a given directory
process_directory() {
    dir="$1"

    for repo in "$dir"/*; do
        if [ -d "$repo/.git" ]; then
            pull_repo "$repo"

            if [ "$NO_SYMLINK" = false ]; then
                create_symlink "$repo"
            fi
        else
            echo "[WARN] Skipping non-repository: $repo" >&2
        fi
    done
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_commands git

    parse_arguments "$@"

    if [ "$ALL" = true ]; then
        process_directory "$HOME/local/github"
        process_directory "$HOME/local/git"
    elif [ "$GITHUB_ONLY" = true ]; then
        process_directory "$HOME/local/github"
    elif [ "$GIT_ONLY" = true ]; then
        process_directory "$HOME/local/git"
    else
        usage
    fi
}

# Execute main function
main "$@"

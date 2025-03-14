#!/bin/sh

########################################################################
# git-all-pull.sh: Git Repositories Pull Script
#
#  Description:
#  This script performs a 'git pull' on all Git repositories in specified
#  local directories. It also checks for and creates symbolic links from
#  the home directory to these repositories.
#
#  WARNING: The '--hard' option performs 'git reset --hard' which can
#  overwrite local changes. Use with caution.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.2 2024-01-07
#       Updated command existence and execution permission checks
#       using a common function for enhanced reliability and maintainability.
#  v1.1 2023-12-07
#       Added check for Git installation and options to pull from specific directories.
#       Added '--all' option and default behavior to show help message if no option is provided.
#       Refactored to comply with POSIX standards.
#  v1.0 2023-12-05
#       Initial release. Supports pulling Git repositories and managing
#       symbolic links with optional arguments.
#
#  Usage:
#  ./git-all-pull.sh [--hard] [--no-symlink] [--dry-run] [--github-only] [--git-only] [--all]
#
########################################################################

HARD_MODE=false
NO_SYMLINK=false
DRY_RUN=false
GITHUB_ONLY=false
GIT_ONLY=false
ALL=false
SHOW_HELP=false

check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Git is not installed. This script requires Git for pulling repositories. Please install Git and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Display script usage information
usage() {
    echo "Usage: $0 [--hard] [--no-symlink] [--dry-run] [--github-only] [--git-only] [--all]"
    echo "Default behavior is to show this help message. Use '--all' to pull from both github and git directories."
    exit 0
}

# Parse options
if [ $# -eq 0 ]; then
    SHOW_HELP=true
fi

for arg in "$@"
do
    case $arg in
        --hard)
        HARD_MODE=true
        ;;
        --no-symlink)
        NO_SYMLINK=true
        ;;
        --dry-run)
        DRY_RUN=true
        ;;
        --github-only)
        GITHUB_ONLY=true
        ;;
        --git-only)
        GIT_ONLY=true
        ;;
        --all)
        ALL=true
        ;;
        *)
        SHOW_HELP=true
        ;;
    esac
done

if [ "$SHOW_HELP" = true ]; then
    usage
fi

# Check if Git is installed
check_commands git

pull_repo() {
    local repo_path="$1"

    if [ "$HARD_MODE" = true ]; then
        if [ "$DRY_RUN" = false ]; then
            echo "Resetting repository: $repo_path"
            git -C "$repo_path" clean -dxf
            git -C "$repo_path" reset --hard
        else
            echo "[DRY RUN] Clear and Reset: $repo_path"
        fi
    fi

    if [ "$DRY_RUN" = false ]; then
        echo "Pulling repository: $repo_path"
        git -C "$repo_path" pull
    else
        echo "[DRY RUN] Pull repository: $repo_path"
    fi
}

create_symlink() {
    local repo_path="$1"
    local base_dir="$2"
    local link_path="$HOME/local/$(basename "$base_dir")/$(basename "$repo_path")"

    cd
    if [ ! -L "$(basename "$repo_path")" ]; then
        if [ "$DRY_RUN" = false ]; then
            echo "Creating symlink: $link_path"
            ln -s "$link_path"
        else
            echo "[DRY RUN] Create symlink: $link_path"
        fi
    fi
}

process_directory() {
    local base_dir="$1"
    for repo_dir in "$base_dir"/*; do
        if [ -d "$repo_dir/.git" ]; then
            pull_repo "$repo_dir"

            if [ "$NO_SYMLINK" = false ]; then
                create_symlink "$repo_dir" "$base_dir"
            fi
        else
            echo "Skipping non-repository: $repo_dir"
        fi
    done
}

# Process each specified directory
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


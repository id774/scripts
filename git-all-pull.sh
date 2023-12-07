#!/bin/sh

########################################################################
# git-all-pull.sh: Git Repositories Pull Script
#
#  Description:
#  This script performs a 'git pull' on all Git repositories in the user's
#  local directories ($HOME/local/git and $HOME/local/github). It also checks for
#  and creates symbolic links from the home directory to these repositories.
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
#  v1.1 2023-12-07
#       Added check for Git installation.
#  v1.0 2023-12-05
#       Initial release. Supports pulling Git repositories and managing
#       symbolic links with optional arguments.
#
#  Usage:
#  ./git-all-pull.sh [--hard] [--no-symlink] [--dry-run]
#
########################################################################

# Check if Git is installed
check_git_installed() {
    if ! command -v git >/dev/null 2>&1; then
        echo "Error: Git is not installed. This script requires Git for pulling repositories. Please install Git and try again."
        exit 1
    fi
}

HARD_MODE=false
NO_SYMLINK=false
DRY_RUN=false

# Parse options
for arg in "$@"
do
    case $arg in
        --hard)
        HARD_MODE=true
        shift
        ;;
        --no-symlink)
        NO_SYMLINK=true
        shift
        ;;
        --dry-run)
        DRY_RUN=true
        shift
        ;;
    esac
done

check_git_installed

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

for base_dir in "$HOME/local/github" "$HOME/local/git"; do
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
done


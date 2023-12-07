#!/bin/sh

########################################################################
# remove-repo.sh: Git Repository Removal Script
#
#  Description:
#  This script removes specified Git repositories from the user's local
#  directories ($HOME/local/github and $HOME/local/git) and any associated
#  symbolic links in the home directory. It checks if the directory is a Git
#  repository before removing it. Includes a dry-run mode for simulation.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2023-12-07
#       Added check to verify if directory is a Git repository.
#       Added dry-run mode and execute option.
#  v1.1 2023-12-05
#       Refactored script with additional comments and error checking.
#  v1.0 2018-04-20
#       Initial release. Removes Git repositories and their symbolic links.
#
#  Usage:
#  ./remove-repo.sh [repository_name...]
#  Add -x to actually execute the removal.
#
########################################################################

DRY_RUN=true

# Check for -x option to execute
for arg in "$@"; do
    if [ "$arg" = "-x" ]; then
        DRY_RUN=false
        break
    fi
done

is_git_repo() {
    [ -d "$1/.git" ]
}

remove_repo() {
    local repo_path_github="$HOME/local/github/$1"
    local repo_path_git="$HOME/local/git/$1"
    local symlink_path="$HOME/$1"

    if [ -d "$repo_path_github" ] && is_git_repo "$repo_path_github"; then
        if [ "$DRY_RUN" = false ]; then
            echo "Removing Git repository: $repo_path_github"
            rm -rf "$repo_path_github"
        else
            echo "[DRY RUN] Removing Git repository: $repo_path_github"
        fi
    fi

    if [ -d "$repo_path_git" ] && is_git_repo "$repo_path_git"; then
        if [ "$DRY_RUN" = false ]; then
            echo "Removing Git repository: $repo_path_git"
            rm -rf "$repo_path_git"
        else
            echo "[DRY RUN] Removing Git repository: $repo_path_git"
        fi
    fi

    if [ -L "$symlink_path" ]; then
        if [ "$DRY_RUN" = false ]; then
            echo "Removing symlink: $symlink_path"
            rm -f "$symlink_path"
        else
            echo "[DRY RUN] Removing symlink: $symlink_path"
        fi
    fi
}

remove_repos() {
    while [ $# -gt 0 ]
    do
        [ "$1" != "-x" ] && remove_repo "$1"
        shift
    done
}

if [ -n "$1" ]; then
    remove_repos "$@"
else
    echo "Usage: $0 [repository_name...] [-x]"
fi


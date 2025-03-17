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
#  v1.3 2025-03-17
#       Encapsulated all logic in functions and introduced main function.
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

# Function to check if a directory is a Git repository
is_git_repo() {
    [ -d "$1/.git" ]
}

remove_from_github() {
    repo_path_github="$HOME/local/github/$repo_name"
    if [ -d "$repo_path_github" ] && is_git_repo "$repo_path_github"; then
        if [ "$DRY_RUN" = false ]; then
            echo "Removing Git repository: $repo_path_github"
            rm -rf "$repo_path_github"
        else
            echo "[DRY RUN] Removing Git repository: $repo_path_github"
        fi
    fi
}

remove_from_git() {
    repo_path_git="$HOME/local/git/$repo_name"
    if [ -d "$repo_path_git" ] && is_git_repo "$repo_path_git"; then
        if [ "$DRY_RUN" = false ]; then
            echo "Removing Git repository: $repo_path_git"
            rm -rf "$repo_path_git"
        else
            echo "[DRY RUN] Removing Git repository: $repo_path_git"
        fi
    fi
}

remove_symlink() {
    symlink_path="$HOME/$repo_name"
    if [ -L "$symlink_path" ]; then
        if [ "$DRY_RUN" = false ]; then
            echo "Removing symlink: $symlink_path"
            rm -f "$symlink_path"
        else
            echo "[DRY RUN] Removing symlink: $symlink_path"
        fi
    fi
}

# Function to remove a Git repository and its symlink
remove_repo() {
    repo_name="$1"

    remove_from_github
    remove_from_git
    remove_symlink
}

# Function to process multiple repositories
remove_repos() {
    while [ $# -gt 0 ]; do
        [ "$1" != "-x" ] && remove_repo "$1"
        shift
    done
}

# Main function to execute the script
main() {
    DRY_RUN=true

    # Check for -x option to execute
    for arg in "$@"; do
        if [ "$arg" = "-x" ]; then
            DRY_RUN=false
            break
        fi
    done

    if [ -n "$1" ]; then
        remove_repos "$@"
    else
        echo "Usage: $0 [repository_name...] [-x]"
    fi
}

# Execute main function
main "$@"

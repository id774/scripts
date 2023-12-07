#!/bin/sh

########################################################################
# remove-repo.sh: Git Repository Removal Script
#
#  Description:
#  This script removes specified Git repositories from the user's local
#  directories ($HOME/local/github and $HOME/local/git) and any associated
#  symbolic links in the home directory. It checks if the directory is a Git
#  repository before removing it.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2023-12-07
#       Added check to verify if directory is a Git repository.
#  v1.1 2023-12-05
#       Refactored script with additional comments and error checking.
#  v1.0 2018-04-20
#       Initial release. Removes Git repositories and their symbolic links.
#
#  Usage:
#  ./remove-repo.sh [repository_name...]
#
########################################################################

is_git_repo() {
    if [ -d "$1/.git" ]; then
        return 0
    else
        return 1
    fi
}

remove_repo() {
    local repo_path_github="$HOME/local/github/$1"
    local repo_path_git="$HOME/local/git/$1"
    local symlink_path="$HOME/$1"

    if is_git_repo "$repo_path_github"; then
        echo "Removing Git repository: $repo_path_github"
        rm -rf "$repo_path_github"
    else
        echo "Skipping: $repo_path_github is not a Git repository."
    fi

    if is_git_repo "$repo_path_git"; then
        echo "Removing Git repository: $repo_path_git"
        rm -rf "$repo_path_git"
    else
        echo "Skipping: $repo_path_git is not a Git repository."
    fi

    if [ -L "$symlink_path" ]; then
        echo "Removing symlink: $symlink_path"
        rm -vf "$symlink_path"
    fi
}

remove_repos() {
    while [ $# -gt 0 ]
    do
        remove_repo "$1"
        shift
    done
}

if [ -n "$1" ]; then
    remove_repos "$@"
else
    echo "Usage: $0 [repository_name...]"
fi


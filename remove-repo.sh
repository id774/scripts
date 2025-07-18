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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./remove-repo.sh [repository_name...]
#
#  Add -x to actually execute the removal.
#
#  Version History:
#  v1.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
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

# Check if a directory is a Git repository.
is_git_repo() {
    [ -d "$1/.git" ]
}

# Remove the Git repository from the github directory if it exists.
remove_from_github() {
    repo_path_github="$HOME/local/github/$repo_name"
    if [ -d "$repo_path_github" ] && is_git_repo "$repo_path_github"; then
        if [ "$DRY_RUN" = false ]; then
            echo "[INFO] Removing Git repository: $repo_path_github"
            rm -rf "$repo_path_github"
        else
            echo "[INFO] DRY RUN: Removing Git repository: $repo_path_github"
        fi
    fi
}

# Remove the Git repository from the git directory if it exists.
remove_from_git() {
    repo_path_git="$HOME/local/git/$repo_name"
    if [ -d "$repo_path_git" ] && is_git_repo "$repo_path_git"; then
        if [ "$DRY_RUN" = false ]; then
            echo "[INFO] Removing Git repository: $repo_path_git"
            rm -rf "$repo_path_git"
        else
            echo "[INFO] DRY RUN: Removing Git repository: $repo_path_git"
        fi
    fi
}

# Remove the symbolic link in the home directory if it exists.
remove_symlink() {
    symlink_path="$HOME/$repo_name"
    if [ -L "$symlink_path" ]; then
        if [ "$DRY_RUN" = false ]; then
            echo "[INFO] Removing symlink: $symlink_path"
            rm -f "$symlink_path"
        else
            echo "[INFO] DRY RUN: Removing symlink: $symlink_path"
        fi
    fi
}

# Remove a Git repository and its related symbolic link.
remove_repo() {
    repo_name="$1"

    remove_from_github
    remove_from_git
    remove_symlink
}

# Iterate through arguments and remove each specified repository.
remove_repos() {
    while [ $# -gt 0 ]; do
        [ "$1" != "-x" ] && remove_repo "$1"
        shift
    done
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

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
        usage
    fi
    return 0
}

# Execute main function
main "$@"
exit $?

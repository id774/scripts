#!/bin/sh

########################################################################
# remove-repo.sh: Git Repository Removal Script
#
#  Description:
#  This script removes specified Git repositories from the user's local
#  directories ($HOME/local/github, $HOME/local/git, and $HOME/local/gitlab)
#  and any associated symbolic links in the home directory. It checks if the
#  directory is a Git repository before removing it. By default, it lists the
#  targets and asks for confirmation before removal. The --dry-run option only
#  previews the targets, and -x removes them without confirmation.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./remove-repo.sh [options] [repository_name...]
#
#  Options:
#      -x            Remove repositories without confirmation
#      --dry-run     Show what would be removed without deleting anything
#      -h, --help    Show this help message
#
#  Examples:
#      ./remove-repo.sh my-repo            # confirm before removal
#      ./remove-repo.sh --dry-run my-repo  # preview only
#      ./remove-repo.sh -x my-repo         # remove without confirmation
#      ./remove-repo.sh repo1 repo2        # confirm once for multiple repositories
#
#  Version History:
#  v2.1 2026-11-04
#       Replace default dry-run with confirmed deletion, retaining -x.
#       Add explicit --dry-run preview mode.
#  v2.0 2026-09-18
#       Remove matching repositories from ~/local/gitlab in addition to existing bases.
#  v1.8 2026-08-18
#       Reject repository names that could escape the configured directories.
#  v1.7 2026-07-11
#       Replace the awk {n,} interval expression in usage() with a portable
#       equivalent, since mawk on some systems matches it incorrectly.
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

# Check if required commands are available and executable
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

# Check if a directory is a Git repository.
is_git_repo() {
    [ -d "$1/.git" ]
}

# Reject names that could resolve outside the repository directories.
validate_repo_name() {
    case "$1" in
        ""|.|..|*/*)
            echo "[ERROR] Invalid repository name: $1" >&2
            return 1
            ;;
    esac
    return 0
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

# Remove the Git repository from the gitlab directory if it exists.
remove_from_gitlab() {
    repo_path_gitlab="$HOME/local/gitlab/$repo_name"
    if [ -d "$repo_path_gitlab" ] && is_git_repo "$repo_path_gitlab"; then
        if [ "$DRY_RUN" = false ]; then
            echo "[INFO] Removing Git repository: $repo_path_gitlab"
            rm -rf "$repo_path_gitlab"
        else
            echo "[INFO] DRY RUN: Removing Git repository: $repo_path_gitlab"
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

    if ! validate_repo_name "$repo_name"; then
        return 1
    fi

    remove_from_github
    remove_from_git
    remove_from_gitlab
    remove_symlink
}

# Ask the user to confirm the removal from the terminal.
confirm_execution() {
    printf '[INFO] Do you want to proceed? [y/N]: '
    if ! read -r response < /dev/tty; then
        echo >&2
        echo '[ERROR] Failed to read the confirmation from the terminal.' >&2
        return 1
    fi

    case "$response" in
        y|Y|yes|Yes|YES) return 0 ;;
        *)
            echo '[ERROR] Aborted by user.' >&2
            return 1
            ;;
    esac
}

# Iterate through arguments and remove each specified repository.
remove_repos() {
    status=0
    while [ $# -gt 0 ]; do
        if [ "$1" != "-x" ] && [ "$1" != "--dry-run" ] && ! remove_repo "$1"; then
            status=1
        fi
        shift
    done
    return "$status"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    if [ -z "$1" ]; then
        usage
    fi

    execute_mode=false
    dry_run_mode=false
    for arg in "$@"; do
        case "$arg" in
            -x) execute_mode=true ;;
            --dry-run) dry_run_mode=true ;;
        esac
    done

    if [ "$execute_mode" = true ] && [ "$dry_run_mode" = true ]; then
        echo "[ERROR] The -x and --dry-run options cannot be used together." >&2
        return 1
    fi

    if [ "$execute_mode" = true ]; then
        DRY_RUN=false
        remove_repos "$@"
        return $?
    fi

    if [ "$dry_run_mode" = true ]; then
        DRY_RUN=true
        remove_repos "$@"
        return $?
    fi

    for arg in "$@"; do
        if ! validate_repo_name "$arg"; then
            return 1
        fi
    done

    DRY_RUN=true
    if ! remove_repos "$@"; then
        return 1
    fi

    if ! confirm_execution; then
        return 1
    fi

    DRY_RUN=false
    remove_repos "$@"
    return $?
}

# Execute main function
main "$@"

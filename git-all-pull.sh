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
#  Usage:
#      ./git-all-pull.sh [--hard] [--no-symlink] [--dry-run]
#          [--list-remote] [--delete-remote-branches]
#          [--github-only] [--git-only] [--www-only] [--all]
#
#  Default behavior is to show this help message. Use '--all' to pull from github, git, and www targets.
#
#  Notes:
#      Specifying both '--github-only' and '--git-only' selects both trees
#      (github and git) and does not include www.
#      '--reset' can be used as an alias of '--hard'.
#      Pulls prune remote-tracking branches deleted from remotes.
#      Pruning does not delete local branches.
#      '--list-remote' lists origin branches except master and main.
#      '--delete-remote-branches' deletes the branches shown by the list option.
#      '--dry-run' previews all operations without changing local or remote state.
#
#  WARNING: The '--hard' option performs 'git reset --hard' which can
#  overwrite local changes. Use with caution.
#  WARNING: The '--delete-remote-branches' option deletes all origin branches
#  except master and main. Review them with '--list-remote' first.
#
#  Version History:
#  v2.4 2026-08-15
#       Add --list-remote to preview origin branches except master and main,
#       and --delete-remote-branches to delete them after review.
#  v2.3 2026-08-07
#       Prune stale remote-tracking branches during repository pulls.
#  v2.2 2026-07-11
#       Replace the awk {n,} interval expression in usage() with a portable
#       equivalent, since mawk on some systems matches it incorrectly.
#  v2.1 2025-12-14
#       Fix symlink creation logic, directory scan condition, and show help for unknown options.
#       Improves robustness without changing functional behavior.
#  v2.0 2025-09-06
#       Add --www-only option to pull /var/www/wordpress and /var/www/html/current when present.
#       Ensure --all also runs www-only processing. Existing options like --dry-run and --hard apply.
#       Add --reset option as an alias of --hard.
#       Add write permission check for repositories and skip.
#  v1.7 2025-08-03
#       Add directory existence check before processing git directories.
#       Improve symlink handling by checking for conflicting existing files.
#       Remove redundant argument parsing from main().
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
########################################################################

# Global variables
HARD_MODE=false
NO_SYMLINK=false
DRY_RUN=false
LIST_REMOTE_BRANCHES=false
DELETE_REMOTE_BRANCHES=false
GITHUB_ONLY=false
GIT_ONLY=false
ALL=false
WWW_ONLY=false
SHOW_HELP=false

# Display full script header information extracted from the top comment block
usage() {
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

# Parse command-line arguments
parse_arguments() {
    for arg in "$@"; do
        case "$arg" in
            --hard) HARD_MODE=true ;;
            --reset) HARD_MODE=true ;;
            --no-symlink) NO_SYMLINK=true ;;
            --dry-run) DRY_RUN=true ;;
            --list-remote) LIST_REMOTE_BRANCHES=true ;;
            --delete-remote-branches) DELETE_REMOTE_BRANCHES=true ;;
            --github-only) GITHUB_ONLY=true ;;
            --git-only) GIT_ONLY=true ;;
            --www-only) WWW_ONLY=true ;;
            --all) ALL=true ;;
            *) SHOW_HELP=true ;;
        esac
    done
}

# Delete remote branches other than master and main from origin
delete_remote_branches() {
    repo="$1"

    if ! remote_heads=$(git -C "$repo" ls-remote --heads origin); then
        echo "[ERROR] Failed to list remote branches: origin ($repo)" >&2
        return 1
    fi

    printf '%s\n' "$remote_heads" | awk '
        $2 ~ /^refs\/heads\// {
            branch = substr($2, 12)
            if (branch != "master" && branch != "main") print branch
        }
    ' | while IFS= read -r branch; do
        [ -n "$branch" ] || continue
        if [ "$DRY_RUN" = true ]; then
            echo "[INFO] DRY RUN: Delete remote branch: origin/$branch ($repo)"
        elif [ "$LIST_REMOTE_BRANCHES" = true ]; then
            echo "[INFO] Remote branch: origin/$branch ($repo)"
        else
            echo "[INFO] Deleting remote branch: origin/$branch ($repo)"
            if ! git -C "$repo" push origin --delete "$branch"; then
                echo "[ERROR] Failed to delete remote branch: origin/$branch ($repo)" >&2
            fi
        fi
    done
}

# Check whether repository is writable
is_repo_writable() {
    repo="$1"
    # Require write access to both the work tree and .git directory
    if [ ! -w "$repo" ] || [ ! -w "$repo/.git" ]; then
        return 1
    fi
    # Best effort: if index.lock exists and is not writable, treat as not writable
    if [ -e "$repo/.git/index.lock" ] && [ ! -w "$repo/.git/index.lock" ]; then
        return 1
    fi
    return 0
}

# Pull updates from a Git repository
pull_repo() {
    repo="$1"

    # Guard: ensure repository is writable before any operation
    if ! is_repo_writable "$repo"; then
        if [ "$DRY_RUN" = true ]; then
            echo "[INFO] DRY RUN: Would skip repository due to no write permission: $repo"
        else
            echo "[WARN] Skipping: $repo (write permission denied)" >&2

        fi
        return 2
    fi

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
        git -C "$repo" pull --prune
    else
        echo "[INFO] DRY RUN: Pull repository: $repo"
    fi

    if [ "$LIST_REMOTE_BRANCHES" = true ] || [ "$DELETE_REMOTE_BRANCHES" = true ]; then
        delete_remote_branches "$repo"
    fi
}

# Create a symbolic link in the home directory for a repository
create_symlink() {
    repo="$1"
    link_path="$HOME/$(basename "$repo")"

    if [ ! -e "$link_path" ]; then
        if [ "$DRY_RUN" = false ]; then
            echo "[INFO] Creating symlink: $link_path -> $repo"
            ln -s "$repo" "$link_path"
        else
            echo "[INFO] DRY RUN: Create symlink: $link_path -> $repo"
        fi
    else
        # Path exists; do not overwrite files or directories
        [ "$DRY_RUN" = true ] && echo "[INFO] DRY RUN: Skip symlink creation due to existing path: $link_path"
    fi
}

# Process all repositories in a given directory
process_directory() {
    dir="$1"

    if [ ! -d "$dir" ]; then
        echo "[INFO] Directory not found (skip as normal): $dir"
        return
    fi

    for repo in "$dir"/*; do
        [ -d "$repo" ] || continue
        if [ -d "$repo/.git" ]; then
            pull_repo "$repo"

            if [ "$NO_SYMLINK" = false ]; then
                create_symlink "$repo"
            fi
        else
            if [ "$DRY_RUN" = true ]; then
                echo "[INFO] DRY RUN: Would skip non-repository: $repo"
            else
                echo "[WARN] Skipping: $repo (not a git repository)" >&2
            fi
        fi
    done
    [ "$SHOW_HELP" = true ] && usage
}

# Process www targets
process_www_only() {
    # List of www paths to process
    for repo in /var/www/wordpress /var/www/html/current; do
        if [ -d "$repo" ]; then
            # Require a git repository to avoid errors on deployed trees
            if [ -d "$repo/.git" ]; then
                pull_repo "$repo"
            else
                if [ "$DRY_RUN" = true ]; then
                    echo "[INFO] DRY RUN: Would skip non-repository: $repo"
                else
                    echo "[WARN] Skipping: $repo (not a git repository)" >&2
                fi
            fi
        else
            # Spec says: if directory does not exist, skip as normal processing (not an error)
            if [ "$DRY_RUN" = true ]; then
                echo "[INFO] DRY RUN: Directory not found (skip as normal): $repo"
            else
                echo "[INFO] Directory not found (skip as normal): $repo"
            fi
        fi
    done
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_commands git

    parse_arguments "$@"

    # Dispatch logic:
    if [ "$ALL" = true ]; then
        process_directory "$HOME/local/github"
        process_directory "$HOME/local/git"
        process_www_only
    else
        ran=false
        if [ "$GITHUB_ONLY" = true ]; then
            process_directory "$HOME/local/github"
            ran=true
        fi
        if [ "$GIT_ONLY" = true ]; then
            process_directory "$HOME/local/git"
            ran=true
        fi
        if [ "$WWW_ONLY" = true ]; then
            process_www_only
            ran=true
        fi
        # If no selector was provided, show help
        if [ "$ran" = false ]; then
            usage
        fi
    fi
    return 0
}

# Execute main function
main "$@"

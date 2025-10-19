#!/bin/sh

########################################################################
# git-symlink.sh: Home Symlink Synchronization Script
#
#  Description:
#  This script force-recreates symbolic links in the home directory for all
#  first-level directories under the specified local bases:
#    - ~/local/github
#    - ~/local/git
#  In default mode, it also purges ALL broken symlinks directly under the home
#  directory. With the uninstall option, it removes home symlinks whose names
#  match first-level directory names under the bases.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./git-symlink.sh [--dry-run] [--github-only] [--git-only] [--all]
#      ./git-symlink.sh -u|--uninstall [--github-only] [--git-only] [--all]
#
#  Default behavior (no selector specified) is to show this help message.
#  Use '--all' to operate on both github and git bases, or select one
#  with '--github-only' or '--git-only'. In default (sync) mode the script:
#    1) Force-creates symlinks in $HOME for each first-level directory
#       under the selected base(s).
#    2) Purges ALL broken symlinks that exist directly under $HOME.
#
#  WARNING:
#    - Force recreation may overwrite existing paths in $HOME. If a path with
#      the same name exists and is not a symlink, it will be removed before
#      creating a new symlink. Back up important files before use.
#    - Uninstall mode removes only symlinks in $HOME whose names match the
#      repositories under the selected base(s). Non-symlinks are not touched.
#
#  Options:
#      --dry-run       Show planned actions without making changes.
#      --github-only   Operate only on ~/local/github.
#      --git-only      Operate only on ~/local/git.
#      --all           Operate on both bases.
#      -u, --uninstall Remove matching symlinks in $HOME (do not recreate).
#
#  Version History:
#  v1.0 2025-09-01
#       Initial release.
#
########################################################################

# Global flags
DRY_RUN=false
GITHUB_ONLY=false
GIT_ONLY=false
ALL=false
UNINSTALL=false

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
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

# Parse and set operation flags from CLI arguments
parse_arguments() {
    # Accepts: --dry-run, --github-only, --git-only, --all, -u/--uninstall
    for arg in "$@"; do
        case "$arg" in
            --dry-run) DRY_RUN=true ;;
            --github-only) GITHUB_ONLY=true ;;
            --git-only) GIT_ONLY=true ;;
            --all) ALL=true ;;
            -u|--uninstall) UNINSTALL=true ;;
            -h|--help|-v|--version) usage ;;
            *) usage ;;
        esac
    done
}

# Build the list of base directories to operate on
select_bases() {
    # Echoes a space-separated list of absolute base paths
    home_dir=$HOME
    github_base="${home_dir}/local/github"
    git_base="${home_dir}/local/git"

    # Selection rules:
    # 1) --all               -> both bases
    # 2) --github-only       -> github base
    # 3) --git-only          -> git base
    # 4) both selectors set  -> both bases
    # 5) no selector         -> return empty to trigger usage()

    if [ "$ALL" = true ]; then
        echo "${github_base} ${git_base}"
        return 0
    fi

    if [ "$GITHUB_ONLY" = true ] && [ "$GIT_ONLY" = true ]; then
        echo "${github_base} ${git_base}"
        return 0
    fi

    if [ "$GITHUB_ONLY" = true ]; then
        echo "${github_base}"
        return 0
    fi

    if [ "$GIT_ONLY" = true ]; then
        echo "${git_base}"
        return 0
    fi

    # No selection given -> empty
    return 0
}

# Create or replace a symlink in $HOME for a given source directory
create_or_replace_link() {
    # $1: absolute path to source directory
    src="$1"
    name=$(basename "$src")
    dest="$HOME/$name"

    # Remove any existing path at destination (symlink or real)
    if [ -e "$dest" ] || [ -L "$dest" ]; then
        if [ "$DRY_RUN" = true ]; then
            echo "[INFO] DRY RUN: Remove existing path: $dest"
        else
            rm -rf -- "$dest"
        fi
    fi

    # Create the symlink
    if [ "$DRY_RUN" = true ]; then
        echo "[INFO] DRY RUN: ln -s \"$src\" \"$dest\""
    else
        ln -s "$src" "$dest"
        echo "[INFO] Linked $dest -> $src"
    fi
}

# Iterate first-level directories under each base and (re)create links
recreate_links_for_bases() {
    # $@: list of base directories
    for base in "$@"; do
        if [ ! -d "$base" ]; then
            echo "[WARN] Base not found: $base" >&2
            continue
        fi
        # List only first-level directories
        find "$base" -mindepth 1 -maxdepth 1 -type d -print | while IFS= read -r d; do
            create_or_replace_link "$d"
        done
    done
}

# Purge all broken symlinks directly under $HOME
purge_broken_symlinks_in_home() {
    # Finds symlinks in $HOME whose targets no longer exist and removes them
    echo "[INFO] Purging broken symlinks in $HOME"
    # Only depth=1 (directly under $HOME) to avoid unintended recursion
    find "$HOME" -mindepth 1 -maxdepth 1 -type l -print | while IFS= read -r l; do
        if [ ! -e "$l" ]; then
            if [ "$DRY_RUN" = true ]; then
                echo "[INFO] DRY RUN: rm -f \"$l\""
            else
                rm -f -- "$l"
                echo "[INFO] Removed broken symlink $l"
            fi
        fi
    done
}

# Uninstall: remove symlinks in $HOME that match names under the bases
uninstall_matching_symlinks() {
    # $@: list of base directories
    removed=0
    for base in "$@"; do
        if [ ! -d "$base" ]; then
            echo "[WARN] Base not found: $base" >&2
            continue
        fi
        find "$base" -mindepth 1 -maxdepth 1 -type d -print | while IFS= read -r d; do
            name=$(basename "$d")
            path="$HOME/$name"
            if [ -L "$path" ]; then
                if [ "$DRY_RUN" = true ]; then
                    echo "[INFO] DRY RUN: rm -f \"$path\""
                else
                    rm -f -- "$path"
                    echo "[INFO] Uninstalled symlink $path"
                fi
                removed=$((removed + 1))
            fi
        done
    done
    echo "[INFO] Uninstall completed."
    echo "[INFO] Removed symlink count may be partially reported due to subshells."
    # Note: Accurate counting across while-pipes is non-trivial in pure POSIX sh.
}

# Main entry point of the script
main() {
    case "${1-}" in
        -h|--help|-v|--version) usage ;;
    esac

    check_commands find rm ln basename

    parse_arguments "$@"

    bases=$(select_bases)

    if [ "$UNINSTALL" = true ]; then
        echo "[INFO] Mode: uninstall matching symlinks"
        # shellcheck disable=SC2086
        uninstall_matching_symlinks $bases
        exit 0
    fi

    # No target selected -> show help as documented
    if [ -z "$bases" ]; then
        usage
    fi

    echo "[INFO] Mode: recreate links and purge broken symlinks"
    # shellcheck disable=SC2086
    recreate_links_for_bases $bases
    purge_broken_symlinks_in_home
    return 0
}

# Execute main function
main "$@"

#!/bin/sh

########################################################################
# git-follow-origin.sh: GitHub Repository Merge Script
#
#  Description:
#  This script automates the process of merging changes from a GitHub
#  repository into the local master branch. It is useful for quickly
#  incorporating updates from a specified user's repository.
#  It checks for Git installation before proceeding.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script with the GitHub user's name and the repository name:
#      ./git-follow-origin.sh <user> <repo>
#
#  Example:
#      ./git-follow-origin.sh exampleUser exampleRepo
#
#  This will merge changes from 'exampleUser/exampleRepo' into the local master branch.
#
#  Version History:
#  v1.9 2025-12-15
#       Detect upstream default branch (main/master) and merge via a unique temp branch.
#       Add sed/date/grep to command checks.
#  v1.8 2025-08-04
#       Fix argument expansion from $* to "$@" to preserve parameter integrity.
#  v1.7 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.6 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.5 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.4 2025-03-17
#       Encapsulated all logic in functions and introduced main function.
#  v1.3 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.2 2024-01-07
#       Updated command existence and execution permission checks
#       using a common function for enhanced reliability and maintainability.
#  v1.1 2023-12-07
#       Added check for Git installation.
#  v1.0 2013-02-05
#       Initial release. Implements functionality to merge a specified
#       GitHub repository's changes into the local master branch.
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

# Resolve upstream default branch name (main/master) with fallback
resolve_upstream_branch() {
    user="$1"
    repo="$2"
    url="https://github.com/${user}/${repo}"

    # Try to read HEAD symref (e.g., "ref: refs/heads/main HEAD")
    head_line=$(git ls-remote --symref "$url" HEAD 2>/dev/null | sed -n '1p')
    if [ -n "$head_line" ]; then
        # Extract branch name after refs/heads/
        br=$(printf "%s\n" "$head_line" | sed -n 's#^ref: refs/heads/\([^ ]*\) HEAD#\1#p')
        if [ -n "$br" ]; then
            echo "$br"
            return 0
        fi
    fi

    # Fallback: check main first, then master (match non-empty output)
    if git ls-remote --heads "$url" main 2>/dev/null | grep -qE '\srefs/heads/main$'; then
        echo "main"
        return 0
    fi
    if git ls-remote --heads "$url" master 2>/dev/null | grep -qE '\srefs/heads/master$'; then
        echo "master"
        return 0
    fi

    # Last resort
    echo "master"
    return 0
}

# Merge upstream changes into local master via a unique temp branch
git_merge() {
    user="$1"
    repo="$2"

    upstream_branch=$(resolve_upstream_branch "$user" "$repo")
    url="https://github.com/${user}/${repo}"

    # Unique temp branch name to avoid collision
    ts=$(date +%Y%m%d%H%M%S)
    temp_branch="merge-${upstream_branch}-${ts}"

    # Create temp branch from local master
    git checkout -b "$temp_branch" master
    # Pull only the resolved upstream branch into temp
    git pull "$url" "$upstream_branch"

    # Merge into local master and push
    git checkout master
    git merge "$temp_branch"
    git push origin master
    git branch -D "$temp_branch"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    if [ -n "$2" ]; then
        # Ensure required commands exist
        check_commands git sed date grep
        git_merge "$@"
    else
        usage
    fi
    return 0
}

# Execute main function
main "$@"

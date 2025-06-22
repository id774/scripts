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
#  Version History:
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
#  Usage:
#  Run the script with the GitHub user's name and the repository name:
#      ./git-follow-origin.sh <user> <repo>
#
#  Example:
#      ./git-follow-origin.sh exampleUser exampleRepo
#
#  This will merge changes from 'exampleUser/exampleRepo' into the local master branch.
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

# Function to check required commands
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

# Processes for git merge
git_merge() {
    git checkout -b merge-master master
    git pull https://github.com/$1/$2
    git checkout master
    git merge merge-master
    git push origin master
    git branch -D merge-master
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    if [ -n "$2" ]; then
        # Check if Git is installed
        check_commands git
        git_merge $*
    else
        usage
    fi
}

# Execute main function
main "$@"

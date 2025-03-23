#!/bin/sh

########################################################################
# git-co-remote-branch.sh: Git Remote Branch Checkout Script
#
#  Description:
#  This script simplifies the process of checking out a remote branch in Git.
#  It automatically creates a new local branch that tracks the specified remote branch.
#  This is useful for quickly switching to and working on branches that exist
#  on the remote repository but not yet locally. It checks for Git installation
#  before proceeding.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.3 2025-03-17
#       Encapsulated all logic in functions and introduced main function.
#  v1.2 2024-01-07
#       Updated command existence and execution permission checks
#       using a common function for enhanced reliability and maintainability.
#  v1.1 2023-12-07
#       Added check for Git installation.
#  v1.0 2016-01-26
#       Initial release. Implements functionality to checkout a remote branch in Git,
#       creating a corresponding local branch that tracks it.
#
#  Usage:
#  Run the script with the name of the remote branch:
#      ./git-co-remote-branch.sh <branch>
#
#  Example:
#      ./git-co-remote-branch.sh feature-branch
#
#  This will create and switch to a local branch called 'feature-branch'
#  that tracks 'origin/feature-branch'.
#
########################################################################

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    # Check if Git is installed
    check_commands git

    if [ -n "$1" ]; then
        check_git_installed
        git checkout -b $1 origin/$1
    else
        usage
    fi
}

# Execute main function
main "$@"

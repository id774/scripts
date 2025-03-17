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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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
#  This will merge changes from 'exampleUser/exampleRepo' into the local master branch.
#
########################################################################

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
    if [ -n "$2" ]; then
        # Check if Git is installed
        check_commands git
        ping -c 1 github.com > /dev/null 2>&1 || exit 1
        git_merge $*
    else
        echo "usage: git-follow-origin <user> <repo>"
        exit 0
    fi
}

# Execute main function
main "$@"

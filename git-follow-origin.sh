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
#  v1.1 2023-12-07
#       Added check for Git installation.
#  v1.0 2013-02-05
#       Initial release. Implements functionality to merge a specified
#       GitHub repository's changes into the local master branch.
#
# Usage:
#  Run the script with the GitHub user's name and the repository name:
#      ./git-follow-origin.sh <user> <repo>
#
#  Example:
#      ./git-follow-origin.sh exampleUser exampleRepo
#  This will merge changes from 'exampleUser/exampleRepo' into the local master branch.
#
########################################################################

# Check if Git is installed
check_git_installed() {
    if ! command -v git >/dev/null 2>&1; then
        echo "Error: Git is not installed. This script requires Git to merge changes from GitHub repositories. Please install Git and try again."
        exit 2
    fi
}

git_merge() {
    git checkout -b merge-master master
    git pull https://github.com/$1/$2
    git checkout master
    git merge merge-master
    git push origin master
    git branch -D merge-master
}

main() {
    if [ -n "$2" ]; then
        check_git_installed
        ping -c 1 github.com > /dev/null 2>&1 || exit 1
        git_merge $*
    else
        echo "usage: git-follow-origin <user> <repo>"
        exit 1
    fi
}

main $*


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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-07
#       Added check for Git installation.
#  v1.0 2016-01-26
#       Initial release. Implements functionality to checkout a remote branch in Git,
#       creating a corresponding local branch that tracks it.
#
# Usage:
#  Run the script with the name of the remote branch:
#      ./git-co-remote-branch.sh <branch>
#
#  Example:
#      ./git-co-remote-branch.sh feature-branch
#  This will create and switch to a local branch called 'feature-branch'
#  that tracks 'origin/feature-branch'.
#
########################################################################

# Check if Git is installed
check_git_installed() {
    if ! command -v git >/dev/null 2>&1; then
        echo "Error: Git is not installed. This script requires Git to checkout remote branches. Please install Git and try again."
        exit 2
    fi
}

main() {
    if [ -n "$1" ]; then
        check_git_installed
        git checkout -b $1 origin/$1
    else
        echo "usage: git-co-remote-branch <branch>"
        exit 1
    fi
}

main $*


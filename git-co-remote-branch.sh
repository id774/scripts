#!/bin/sh
#
########################################################################
# git-co-remote-branch: Git Remote Branch Checkout Script
#
#  Description:
#  This script simplifies the process of checking out a remote branch in Git.
#  It automatically creates a new local branch that tracks the specified remote branch.
#  This is useful for quickly switching to and working on branches that exist
#  on the remote repository but not yet locally.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 1/26,2016
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

main() {
    if [ -n "$1" ]; then
        git checkout -b $1 origin/$1
    else
        echo "usage: git-co-remote-branch <branch>"
        exit 1
    fi
}

main $*

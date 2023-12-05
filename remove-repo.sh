#!/bin/sh

########################################################################
# remove-repo.sh: Git Repository Removal Script
#
#  Description:
#  This script removes specified Git repositories from the user's local
#  directories ($HOME/local/github and $HOME/local/git) and any associated
#  symbolic links in the home directory.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-05
#       Refactored script with additional comments and error checking.
#  v1.0 2018-04-20
#       Initial release. Removes Git repositories and their symbolic links.
#
#  Usage:
#  ./remove-repo.sh [repository_name...]
#
########################################################################

remove_repo() {
    if [ -d "$HOME/local/github/$1" ]; then
        echo "Removing repository: $HOME/local/github/$1"
        rm -rf "$HOME/local/github/$1"
    fi

    if [ -d "$HOME/local/git/$1" ]; then
        echo "Removing repository: $HOME/local/git/$1"
        rm -rf "$HOME/local/git/$1"
    fi

    if [ -L "$HOME/$1" ]; then
        echo "Removing symlink: $HOME/$1"
        rm -vf "$HOME/$1"
    fi
}

remove_repos() {
    while [ $# -gt 0 ]
    do
        remove_repo "$1"
        shift
    done
}

if [ -n "$1" ]; then
    remove_repos "$@"
else
    echo "Usage: $0 [repository_name...]"
fi


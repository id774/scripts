#!/bin/sh

########################################################################
# Following origin master
#  $1 = user
#  $2 = host
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2/4,2013
#       First.
########################################################################

setup_environment() {
    GITBARE_USER=$1
    GITBARE_HOST=$2
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
    test -d $HOME/local/github || mkdir -p $HOME/local/github
    test -d $HOME/local/git || mkdir -p $HOME/local/git
    git_merge $*
}

test -n "$2" || exit 1
setup_environment $*
ping -c 1 id774.net > /dev/null 2>&1 || exit 1
git_merge $*

#!/bin/sh

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
    if [ -n "$2" ]; then
        setup_environment $*
        ping -c 1 id774.net > /dev/null 2>&1 || exit 1
        test -d $HOME/local/github || mkdir -p $HOME/local/github
        test -d $HOME/local/git || mkdir -p $HOME/local/git
        git_merge $*
    else
        echo "usage: git-follow-origin <user> <repo>"
        exit 1
    fi
}

main $*

#!/bin/sh

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
        ping -c 1 github.com > /dev/null 2>&1 || exit 1
        git_merge $*
    else
        echo "usage: git-follow-origin <user> <repo>"
        exit 1
    fi
}

main $*

#!/bin/sh

main() {
    if [ -n "$1" ]; then
        git checkout -b $1 origin/$1
    else
        echo "usage: git-co-remote-branch <branch>"
        exit 1
    fi
}

main $*

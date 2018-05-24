#!/bin/sh

remove_repo() {
    test -d $HOME/local/github/$1 && rm -rf $HOME/local/github/$1
    test -d $HOME/local/git/$1 && rm -rf $HOME/local/git/$1
    test -L $HOME/$1 && rm -vf $HOME/$1
}

remove_repos() {
    while [ $# -gt 0 ]
    do
        remove_repo $1
        shift
    done
}

test -n "$1" && remove_repos $*

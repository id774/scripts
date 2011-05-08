#!/bin/sh

########################################################################
# Pulling all private repository.
#  $1 = user
#  $2 = host
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v2.0 4/15,2011
#       Repositories are moved from github to my server.
#  v1.0 7/26,2010
#       Stable.
########################################################################

setup_environment() {
    GITBARE_USER=$1
    GITBARE_HOST=$2
}

remove_github_repo() {
    if [ -d $HOME/local/github/$1 ]; then
        echo "Purge $1 on github repository"
        rm -rf $HOME/local/github/$1
    fi
}

pull_from_gitbare() {
    remove_github_repo $2
    echo "Pulling $1 $2 $5"
    if [ -e $HOME/local/$1/$2 ]; then
        cd $HOME/local/$1/$2
        test -n "$5" && git reset --hard
        git pull
    else
        cd $HOME/local/$1
        git clone ssh://$GITBARE_USER@$GITBARE_HOST/~$GITBARE_USER/git-bare/$2.git
    fi
    chmod 700 $HOME/local/$1/$2
    test -L $HOME/$2 && rm $HOME/$2
    ln -fs $HOME/local/$1/$2 $HOME/$2
}

gitpull_all() {
    pull_from_gitbare git private $*
    pull_from_gitbare git development $*
}

test -n "$2" || exit 2
setup_environment $*
ping -c 1 id774.net > /dev/null 2>&1 || exit 1
gitpull_all $*

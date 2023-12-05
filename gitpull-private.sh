#!/bin/sh

########################################################################
# Pulling all private repository.
#  $1 = user
#  $2 = host
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v2.3 2016-01-28
#       Add ifexist option.
#  v2.2 2014-12-17
#       Improvement symlink confirmation process.
#  v2.1 2014-03-02
#       Specify default gituser and githost.
#  v2.0 2011-04-15
#       Repositories are moved from github to my server.
#  v1.0 2010-07-26
#       Stable.
########################################################################

setup_environment() {
    test -n "$1" || GITBARE_USER=git
    test -n "$1" && GITBARE_USER=$1
    test -n "$2" || GITBARE_HOST=git.id774.net
    test -n "$2" && GITBARE_HOST=$2
}

remove_github_repo() {
    if [ -d $HOME/local/github/$1 ]; then
        echo "Purge $1 on github repository"
        rm -rf $HOME/local/github/$1
    fi
}

set_permission() {
    chmod 700 $HOME/local/$1/$2
    test -L $HOME/$2 || ln -fs $HOME/local/$1/$2 $HOME/$2
}

pull_repo() {
    echo "Pulling $1 $2 $5"
    cd $HOME/local/$1/$2
    test "$5" = "--hard" && git reset --hard
    git pull
    set_permission $*
}

clone_repo() {
    echo "Cloning $1 $2 $5"
    cd $HOME/local/$1
    git clone ssh://$GITBARE_USER@$GITBARE_HOST/var/lib/git/$2.git
    set_permission $*
}

pull_from_gitbare() {
    remove_github_repo $2
    if [ -e $HOME/local/$1/$2 ]; then
        pull_repo $*
    else
        if [ ! "$5" = "--ifexist" ]; then
            clone_repo $*
        fi
    fi
}

gitpull_all() {
    pull_from_gitbare git private $*
    pull_from_gitbare git development $*
    pull_from_gitbare git data $*
    pull_from_gitbare git kabbala $*
    pull_from_gitbare git goodstory $*
    pull_from_gitbare git service_portal $*
    pull_from_gitbare git redmine $*
    pull_from_gitbare git news_cloud $*
    pull_from_gitbare git newscloud-sinatra $*
    pull_from_gitbare git classify $*
}

setup_environment $*
ping -c 1 id774.net > /dev/null 2>&1 || exit 1
gitpull_all $*

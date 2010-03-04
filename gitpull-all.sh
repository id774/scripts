#!/bin/sh

gitpull() {
    if [ -d $HOME/local/$1/$3 ]; then
        cd ~/local/$1/$3
        git pull
    else
        cd ~/local/$1
        git clone git://github.com/$2/$3.git
    fi
}

assembla_git() {
    if [ -d $HOME/local/$1/$2 ]; then
        cd ~/local/$1/$2
        git pull
    else
        cd ~/local/$1
        git clone git://git.assembla.com/$2.git
    fi
}

emacswiki_get() {
    test -d $HOME/local/$1 || mkdir -p $HOME/local/$1
    cd ~/local/$1
    test -f $1.el && rm $1.el
    wget http://www.emacswiki.org/emacs/download/$1.el
    diff ~/local/$1/$1.el ~/.emacs.d/elisp/3rd-party/$1.el
}

repoorcz_pull() {
    if [ -d $HOME/local/$1/$2 ]; then
        cd ~/local/$1/$2
        $1 pull
    else
        cd ~/local/$1
        git clone git://repo.or.cz/$2.git
    fi
}

anything_get_all() {
    repoorcz_pull git anything-config
    emacswiki_get anything
}

gitpull_all() {
    gitpull github id774 id774.github.com
    gitpull github hayamiz twittering-mode
    gitpull github miyagawa plagger
}

assembla_git_all() {
    assembla_git git 774-gthumb
}

anything_get_all
gitpull_all
assembla_git_all

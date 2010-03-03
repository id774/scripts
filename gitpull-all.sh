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

gitpull github id774 id774.github.com
gitpull github hayamiz twittering-mode
gitpull github miyagawa plagger


#!/bin/sh

########################################################################
# Pulling all repository.
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 12/17,2014
#       Add Heroku.
#       Improvement symlink confirmation process.
#  v1.1 8/7,2013
#       Speficy source address when git pull.
#  v1.0 7/26,2010
#       Stable.
########################################################################

gitpull() {
    echo "Pulling $1 $3 $4"
    if [ -e $HOME/local/$1/$3 ]; then
        cd $HOME/local/$1/$3
        test -n "$4" && git reset --hard
        git pull
    else
        cd $HOME/local/$1
        git clone git://github.com/$2/$3.git
    fi
    test -L $HOME/$3 || ln -fs $HOME/local/$1/$3 $HOME/$3
}

heroku_git() {
    echo "Pulling $1 $2 $3"
    if [ -e $HOME/local/$1/$2 ]; then
        cd $HOME/local/$1/$2
        test -n "$3" && git reset --hard
        git pull
    else
        cd $HOME/local/$1
        git clone git@heroku.com:$2.git
    fi
    test -L $HOME/$2 || ln -fs $HOME/local/$1/$2 $HOME/$2
}

assembla_git() {
    echo "Pulling $1 $2 $3"
    if [ -d $HOME/local/$1/$2 ]; then
        cd $HOME/local/$1/$2
        test -n "$3" && git reset --hard
        git pull
    else
        cd $HOME/local/$1
        git clone git://git.assembla.com/$2.git
    fi
    test -L $HOME/$2 || ln -fs $HOME/local/$1/$2 $HOME/$2
}

debian_monthly_report() {
    echo "Pulling $1 $2 $3"
    if [ -d $HOME/local/$1/$2 ]; then
        cd $HOME/local/$1/$2
        test -n "$3" && git reset --hard
        $1 pull
    else
        cd $HOME/local/$1
        git clone git://git.debian.org/$1/tokyodebian/$2.git
        cd monthly-report
    fi
    cp -p git-pre-commit.sh .git/hooks/pre-commit
    test -L $HOME/$2 || ln -fs $HOME/local/$1/$2 $HOME/$2
}

emacswiki_get() {
    echo "Pulling $1"
    test -d $HOME/local/$1 || mkdir -p $HOME/local/$1
    cd $HOME/local/$1
    test -f $1.el && rm $1.el
    wget http://www.emacswiki.org/emacs/download/$1.el
    diff $HOME/local/$1/$1.el $HOME/.emacs.d/elisp/3rd-party/$1.el
    test -L $HOME/$1 || ln -fs $HOME/local/$1/$1 $HOME/$1
}

repoorcz_pull() {
    echo "Pulling $1 $2 $3"
    if [ -d $HOME/local/$1/$2 ]; then
        cd $HOME/local/$1/$2
        test -n "$3" && git reset --hard
        $1 pull
    else
        cd $HOME/local/$1
        git clone git://repo.or.cz/$2.git
    fi
    test -L $HOME/$2 || ln -fs $HOME/local/$1/$2 $HOME/$2
}

anything_get_all() {
    repoorcz_pull git anything-config $*
    #emacswiki_get anything
}

assembla_git_all() {
    assembla_git git 774-gthumb $*
}

gitpull_all() {
    gitpull github id774 dot_zsh $*
    gitpull github id774 dot_emacs $*
    gitpull github id774 automaticruby $*
    gitpull github id774 termtter $*
    gitpull github id774 termtter-plugins $*
    gitpull github id774 munin-plugins $*
    gitpull github id774 deferred-sync $*
    gitpull github id774 stdout $*
    gitpull github id774 sysadmin $*
    gitpull github id774 instant-deployer $*
    gitpull github id774 twitter_viewer $*
    gitpull github id774 blog_viewer $*
    gitpull github id774 repo_manager $*
    gitpull github id774 abuse $*
    gitpull github id774 naivebayes $*
    gitpull github id774 kmeans $*
    gitpull github id774 recommendation $*
    gitpull github id774 vocabulary $*
    gitpull github id774 depression $*
    gitpull github id774 numerology $*
    gitpull github id774 sandbox $*
    gitpull github id774 hotnews $*
    gitpull github id774 finance $*
    gitpull github id774 list_shift $*
    gitpull github id774 rurima $*
    gitpull github id774 batch_framework $*
    gitpull github id774 intraweb-template $*
    gitpull github id774 heartbeat-id774net $*
    gitpull github id774 hadoop-streaming $*
    gitpull github id774 d3js-charts $*
    gitpull github id774 ctoD $*
    gitpull github id774 okura $*
    gitpull github id774 flask-bootstrap $*
    gitpull github id774 rails4-bootstrap $*
    gitpull github id774 sinatra-bootstrap $*
    gitpull github id774 sinatra-api-provider $*
    gitpull github id774 fluentd-json-receiver $*
    gitpull github id774 house_api_web $*
    gitpull github id774 akazunomabot $*
    gitpull github id774 scipy-lecture-notes $*
    gitpull github fastladder fastladder $*
    gitpull github jugyo rubytter $*
    gitpull github fluent fluentd $*
    gitpull github sandal rbp $*
    gitpull github emacs-helm helm $*
    gitpull github eschulte rhtml $*
    gitpull github eschulte rinari $*
    gitpull github auto-complete auto-complete $*
    gitpull github auto-complete popup-el $*
    gitpull github auto-complete fuzzy-el $*
    gitpull github kiwanami emacs-deferred
    gitpull github hitode909 emacs-highlight-unique-symbol
    gitpull github rooney zencoding $*
    gitpull github hober html5-el $*
    gitpull github nex3 haml-mode $*
    gitpull github nex3 sass-mode $*
    gitpull github antonj scss-mode $*
    gitpull github defunkt coffee-mode $*
    gitpull github mooz js2-mode $*
    gitpull github mooz shadow.el $*
    gitpull github ujihisa shadow.vim $*
    gitpull github othree html5.vim $*
    gitpull github kchmck vim-coffee-script $*
    gitpull github tpope vim-haml $*
    heroku_git git d3js-data-clips $*
    heroku_git git d3js-stacked-chart $*
}

main() {
    test -d $HOME/local/github || mkdir -p $HOME/local/github
    test -d $HOME/local/git || mkdir -p $HOME/local/git
    #debian_monthly_report git monthly-report $*
    #assembla_git_all $*
    anything_get_all $*
    gitpull_all $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*

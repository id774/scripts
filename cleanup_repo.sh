#!/bin/sh

remove_repo() {
    test -d $HOME/local/github/$1 && rm -rf $HOME/local/github/$1
    test -d $HOME/local/git/$1 && rm -rf $HOME/local/git/$1
    test -L $HOME/$1 && rm -vf $HOME/$1
}

clean_repo() {
    while [ $# -gt 0 ]
    do
        remove_repo $1
        shift
    done
}

clean_repo automaticruby termtter termtter-plugins stdout sysadmin instant-deployer twitter_viewer blog_viewer repo_manager abuse naivebayes kmeans recommendation vocabulary depression numerology hotnews list_shift rurima batch_framework hadoop-streaming heartbeat-id774net ctoD okura rails4-bootstrap rails5-bootstrap sinatra-api-provider sinatra-bootstrap fluentd-json-receiver house_api_web scipy-lecture-notes rubytter fluentd rbp helm rhtml rinari popup-el fuzzy-el html5-el haml-mode sass-mode scss-mode coffee-mode js2-mode shadow.el shadow.vim html5.vim vim-coffee-script vim-ham anything-config munin-plugins deferred-sync intraweb-template d3js-charts flask-hello flask-bootstrap auto-complete emacs-deferred emacs-highlight-unique-symbol zencoding vim-haml

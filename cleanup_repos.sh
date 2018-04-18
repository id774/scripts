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

clean_repo \
  abuse \
  anything-config \
  auto-complete \
  automaticruby \
  batch_framework \
  blog_viewer \
  classify \
  coffee-mode \
  ctoD \
  d3js-charts \
  d3js-data-clips \
  d3js-stacked-chart \
  deferred-sync \
  depression \
  emacs-deferred \
  emacs-highlight-unique-symbol \
  flask-bootstrap \
  flask-hello \
  fluentd \
  fluentd-json-receiver \
  fuzzy-el \
  goodstory \
  hadoop-streaming \
  haml-mode \
  heartbeat-id774net \
  helm \
  hotnews \
  house_api_web \
  html5-el \
  html5.vim \
  instant-deployer \
  intraweb-template \
  js2-mode \
  kmeans \
  list_shift \
  munin-plugins \
  naivebayes \
  news_cloud \
  newscloud-sinatra \
  numerology \
  okura \
  popup-el \
  rails4-bootstrap \
  rails5-bootstrap \
  rbp \
  recommendation \
  redmine \
  repo_manager \
  rhtml \
  rinari \
  rubytter \
  rurima \
  sass-mode \
  scipy-lecture-notes \
  scss-mode \
  service_portal \
  shadow.el \
  shadow.vim \
  sinatra-api-provider \
  sinatra-bootstrap \
  stdout \
  sysadmin \
  termtter \
  termtter-plugins \
  twitter_viewer \
  vim-coffee-script \
  vim-ham \
  vim-haml \
  vocabulary \
  weekly-report \
  wordpress \
  zencoding

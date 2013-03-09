#!/bin/sh
#
########################################################################
# Install gem packages
#  $1 = ruby path (ex. /usr/local)
#  $2 $3 $4 = proxy
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v2.0 10/31,2012
#       Add some gems.
#  v1.0 8/15,2008
#       Stable.
########################################################################

install_gem() {
    test -n "$1" && export GEM=$1/bin/gem
    test -n "$1" || export GEM=gem
    export RUBYOPT=rubygems
    sudo $GEM update --system $2 $3 $4
    sudo $GEM install pry $2 $3 $4
    sudo $GEM install rake $2 $3 $4
    sudo $GEM install bundler $2 $3 $4
    sudo $GEM install minitest $2 $3 $4
    sudo $GEM install rails $2 $3 $4
    sudo $GEM install sqlite3 $2 $3 $4
    sudo $GEM install sass $2 $3 $4
    sudo $GEM install sass-rails $2 $3 $4
    sudo $GEM install coffee-script $2 $3 $4
    sudo $GEM install coffee-rails $2 $3 $4
    sudo $GEM install jquery-rails $2 $3 $4
    sudo $GEM install haml $2 $3 $4
    sudo $GEM install haml-rails $2 $3 $4
    sudo $GEM install dalli $2 $3 $4
    sudo $GEM install backbone-rails $2 $3 $4
    sudo $GEM install uglifier $2 $3 $4
    sudo $GEM install devise '-v < 2' $2 $3 $4
    sudo $GEM install kaminari $2 $3 $4
    sudo $GEM install i18n_generators $2 $3 $4
    sudo $GEM install jeweler $2 $3 $4
    sudo $GEM install will_paginate $2 $3 $4
    sudo $GEM install meta_search $2 $3 $4
    sudo $GEM install enumerable-lazy $2 $3 $4
    sudo $GEM install term-ansicolor $2 $3 $4
    sudo $GEM install turn $2 $3 $4
    sudo $GEM install hpricot $2 $3 $4
    sudo $GEM install mechanize $2 $3 $4
    sudo $GEM install nokogiri $2 $3 $4
    sudo $GEM install Selenium $2 $3 $4
    sudo $GEM install pg $2 $3 $4
    sudo $GEM install postgres $2 $3 $4
    sudo $GEM install mysql2 $2 $3 $4
    sudo $GEM install BlueCloth $2 $3 $4
    sudo $GEM install RedCloth $2 $3 $4
    sudo $GEM install net-ssh $2 $3 $4
    sudo $GEM install net-sftp $2 $3 $4
    sudo $GEM install coverage $2 $3 $4
    sudo $GEM install capistrano $2 $3 $4
    sudo $GEM install redgreen $2 $3 $4
    sudo $GEM install minitest $2 $3 $4
    sudo $GEM install rspec $2 $3 $4
    sudo $GEM install rspec-rails $2 $3 $4
    sudo $GEM install flexmock $2 $3 $4
    sudo $GEM install simplecov $2 $3 $4
    sudo $GEM install simplecov-rcov $2 $3 $4
    sudo $GEM install thrift $2 $3 $4
    sudo $GEM install json $2 $3 $4
    sudo $GEM install rdoc $2 $3 $4
    sudo $GEM install gherkin $2 $3 $4
    sudo $GEM install cucumber $2 $3 $4
    sudo $GEM install sqlite3-ruby $2 $3 $4
    sudo $GEM install -v 1.4.2 rubytter $2 $3 $4
    sudo $GEM install -v 1.11.0 termtter $2 $3 $4
    sudo $GEM install userstream $2 $3 $4
    sudo $GEM install webtail $2 $3 $4
    sudo $GEM install zipruby $2 $3 $4
    sudo $GEM install oauth $2 $3 $4
    sudo $GEM install webrat $2 $3 $4
    sudo $GEM install diff-lcs $2 $3 $4
    sudo $GEM install sanitize $2 $3 $4
    sudo $GEM install msgpack $2 $3 $4
    sudo $GEM install msgpack-rpc $2 $3 $4
    sudo $GEM install feed-normalizer $2 $3 $4
    sudo $GEM install gettext_rails $2 $3 $4
    sudo $GEM install gettext_activerecord $2 $3 $4
    sudo $GEM install searchlogic $2 $3 $4
    sudo $GEM install fastercsv $2 $3 $4
    sudo $GEM install log4r $2 $3 $4
    sudo $GEM install sinatra $2 $3 $4
    sudo $GEM install rubytree $2 $3 $4
    sudo $GEM install shoulda $2 $3 $4
    sudo $GEM install tzinfo $2 $3 $4
    sudo $GEM install guard-coffeescript $2 $3 $4
    sudo $GEM install rb-fsevent $2 $3 $4
    sudo $GEM install -v 0.4.2 i18n $2 $3 $4
    sudo $GEM install execjs $2 $3 $4
    sudo $GEM install therubyracer $2 $3 $4
    sudo $GEM install feedbag $2 $3 $4
    sudo $GEM install hashie $2 $3 $4
    sudo $GEM install gcalapi $2 $3 $4
    sudo $GEM install xml-simple $2 $3 $4
    sudo $GEM install prawn $2 $3 $4
    sudo $GEM install gnuplot $2 $3 $4
    sudo $GEM install ai4r $2 $3 $4
    sudo $GEM install kmeans $2 $3 $4
    sudo $GEM install sysadmin $2 $3 $4
    sudo $GEM install automatic $2 $3 $4
    sudo $GEM install rubypython $2 $3 $4
    sudo $GEM install narray $2 $3 $4
    sudo $GEM install gsl $2 $3 $4
    sudo $GEM install rmagick $2 $3 $4
    sudo $GEM install wukong $2 $3 $4
    sudo $GEM install woothee $2 $3 $4
    #sudo $GEM install hadoop-papyrus $2 $3 $4
    #sudo $GEM install jruby-on-hadoop $2 $3 $4
    sudo $GEM install fluentd $2 $3 $4
    sudo $GEM install passenger $2 $3 $4
    $GEM list --local
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_gem $*

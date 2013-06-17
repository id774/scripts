#!/bin/sh
#
########################################################################
# Install gem packages
#  $1 = ruby path (ex. /usr/local)
#  $2 $3 $4 = proxy
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v2.1 4/21,2013
#       --no-ri --no-rdoc option as default.
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
    sudo $GEM install pry --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install rake --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 1.3.5 bundler --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 3.0.4 builder --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install minitest --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 3.2.13 rails --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install sqlite3 --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install sass --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install sass-rails --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install coffee-script --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install coffee-rails --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install jquery-rails --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install haml --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install haml-rails --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install dalli --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install backbone-rails --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install uglifier --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install devise '-v < 2' --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install kaminari --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install i18n_generators --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install jeweler --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install will_paginate --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install meta_search --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install rinku --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install enumerable-lazy --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install term-ansicolor --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install turn --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install hpricot --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install mechanize --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install nokogiri --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install Selenium --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install pg --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install postgres --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install mysql2 --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install cassandra --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 1.9.0 bson_ext --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 1.9.0 mongo --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install BlueCloth --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install RedCloth --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install net-ssh --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install net-scp --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install net-sftp --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install coverage --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install capistrano --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install redgreen --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install minitest --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install rspec --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install rspec-rails --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install flexmock --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install simplecov --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install simplecov-rcov --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install sequel --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install thrift --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install json --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install rdoc --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install gherkin --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install cucumber --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install sqlite3-ruby --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install termtter --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install userstream --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install webtail --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install zipruby --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install oauth --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install webrat --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install diff-lcs --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install sanitize --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 0.5.5 msgpack --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install msgpack-rpc --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install feed-normalizer --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install gettext_rails --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install gettext_activerecord --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install searchlogic --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install fastercsv --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install log4r --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install sinatra --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install rubytree --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install shoulda --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install tzinfo --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install guard-coffeescript --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install rb-fsevent --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install iconv --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 0.4.2 i18n --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install execjs --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install therubyracer --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install feedbag --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install hashie --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install gcalapi --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install xml-simple --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install pocket-ruby --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install weather_hacker --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install when_exe --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install prawn --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install gnuplot --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install ai4r --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install kmeans --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install sysadmin --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install automatic --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install rubypython --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install narray --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install gsl --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install rmagick --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install wukong --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install woothee --no-ri --no-rdoc $2 $3 $4
    #sudo $GEM install hadoop-papyrus --no-ri --no-rdoc $2 $3 $4
    #sudo $GEM install jruby-on-hadoop --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install passenger --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 0.10.35 fluentd --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install fluent-logger-ruby --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install fluent-plugin-sqlite3 --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install fluent-plugin-s3 --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install fluent-plugin-parser --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install fluent-plugin-rewrite-tag-filter --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install fluent-plugin-flowcounter --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install fluent-plugin-growthforecast --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install fluent-plugin-datacounter --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install fluent-plugin-twitter --no-ri --no-rdoc $2 $3 $4
    $GEM list --local
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_gem $*

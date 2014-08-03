#!/bin/sh
#
########################################################################
# Install gem packages
#  $1 = ruby path (ex. /usr/local)
#  $2 = no sudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v2.3 7/8,2014
#       Auto proxy detection.
#  v2.2 1/30,2014
#       No sudo option.
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
    test -n "$2" || SUDO=sudo
    test -n "$2" && SUDO=
    test "$2" = "sudo" && SUDO=sudo
    test -n "$HTTP_PROXY" || PROXY=
    test -n "$HTTP_PROXY" && PROXY="-r -p $HTTP_PROXY"
    export RUBYOPT=rubygems
    $SUDO $GEM update --system $PROXY
    $SUDO $GEM install rb-readline --no-ri --no-rdoc $PROXY
    $SUDO $GEM install debugger --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 0.9" pry --no-ri --no-rdoc $PROXY
    $SUDO $GEM install pry-debugger --no-ri --no-rdoc $PROXY
    $SUDO $GEM install pry-doc --no-ri --no-rdoc $PROXY
    $SUDO $GEM install pry-rails --no-ri --no-rdoc $PROXY
    $SUDO $GEM install pry-coolline --no-ri --no-rdoc $PROXY
    $SUDO $GEM install hirb --no-ri --no-rdoc $PROXY
    $SUDO $GEM install hirb-unicode --no-ri --no-rdoc $PROXY
    $SUDO $GEM install awesome_print --no-ri --no-rdoc $PROXY
    $SUDO $GEM install rake --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 1.3.5" bundler --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 3.1.0" builder --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 3.2.17" rails --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 4.1.1" rails --no-ri --no-rdoc $PROXY
    $SUDO $GEM install sqlite3 --no-ri --no-rdoc $PROXY
    $SUDO $GEM install sqlite3-ruby --no-ri --no-rdoc $PROXY
    $SUDO $GEM install sass --no-ri --no-rdoc $PROXY
    $SUDO $GEM install sass-rails --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 2.0.alpha.0" compass-rails --no-ri --no-rdoc $PROXY
    $SUDO $GEM install coffee-script --no-ri --no-rdoc $PROXY
    $SUDO $GEM install coffee-rails --no-ri --no-rdoc $PROXY
    $SUDO $GEM install jquery-rails --no-ri --no-rdoc $PROXY
    $SUDO $GEM install haml --no-ri --no-rdoc $PROXY
    $SUDO $GEM install haml-rails --no-ri --no-rdoc $PROXY
    $SUDO $GEM install dalli --no-ri --no-rdoc $PROXY
    $SUDO $GEM install backbone-rails --no-ri --no-rdoc $PROXY
    $SUDO $GEM install uglifier --no-ri --no-rdoc $PROXY
    $SUDO $GEM install shotgun --no-ri --no-rdoc $PROXY
    $SUDO $GEM install rails-clean-logs --no-ri --no-rdoc $PROXY
    $SUDO $GEM install devise '-v < 2' --no-ri --no-rdoc $PROXY
    $SUDO $GEM install devise '~> 3' --no-ri --no-rdoc $PROXY
    $SUDO $GEM install i18n_generators --no-ri --no-rdoc $PROXY
    $SUDO $GEM install jeweler --no-ri --no-rdoc $PROXY
    $SUDO $GEM install gemcutter --no-ri --no-rdoc $PROXY
    $SUDO $GEM install kaminari '~> 0.15.0' --no-ri --no-rdoc $PROXY
    $SUDO $GEM install kaminari-bootstrap '~> 0.1.3' --no-ri --no-rdoc $PROXY
    $SUDO $GEM install rinku --no-ri --no-rdoc $PROXY
    #$SUDO $GEM install enumerable-lazy --no-ri --no-rdoc $PROXY
    $SUDO $GEM install term-ansicolor --no-ri --no-rdoc $PROXY
    $SUDO $GEM install turn --no-ri --no-rdoc $PROXY
    $SUDO $GEM install rest-client --no-ri --no-rdoc $PROXY
    $SUDO $GEM install http_configuration --no-ri --no-rdoc $PROXY
    $SUDO $GEM install hpricot --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 2.1.1" mechanize --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 1.5.11" nokogiri --no-ri --no-rdoc $PROXY
    $SUDO $GEM install anemone --no-ri --no-rdoc $PROXY
    $SUDO $GEM install cosmicrawler --no-ri --no-rdoc $PROXY
    $SUDO $GEM install sanitize --no-ri --no-rdoc $PROXY
    $SUDO $GEM install Selenium --no-ri --no-rdoc $PROXY
    $SUDO $GEM install pg --no-ri --no-rdoc $PROXY
    $SUDO $GEM install mysql2 --no-ri --no-rdoc $PROXY
    $SUDO $GEM install cassandra --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 1.9.0" bson --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 1.9.0" bson_ext --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 1.9.0" mongo --no-ri --no-rdoc $PROXY
    #$SUDO $GEM install BlueCloth --no-ri --no-rdoc $PROXY
    #$SUDO $GEM install RedCloth --no-ri --no-rdoc $PROXY
    $SUDO $GEM install net-ssh --no-ri --no-rdoc $PROXY
    $SUDO $GEM install net-scp --no-ri --no-rdoc $PROXY
    $SUDO $GEM install net-sftp --no-ri --no-rdoc $PROXY
    $SUDO $GEM install coverage --no-ri --no-rdoc $PROXY
    $SUDO $GEM install unicorn --no-ri --no-rdoc $PROXY
    $SUDO $GEM install capistrano --no-ri --no-rdoc $PROXY
    $SUDO $GEM install redgreen --no-ri --no-rdoc $PROXY
    $SUDO $GEM install minitest --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 2.14.1" rspec --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 2.14.1" rspec-core --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 2.14.1" rspec-expectations --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 2.14.1" rspec-mocks --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 2.14.1" rspec-rails --no-ri --no-rdoc $PROXY
    $SUDO $GEM install flexmock --no-ri --no-rdoc $PROXY
    $SUDO $GEM install simplecov --no-ri --no-rdoc $PROXY
    $SUDO $GEM install simplecov-rcov --no-ri --no-rdoc $PROXY
    $SUDO $GEM install sequel --no-ri --no-rdoc $PROXY
    $SUDO $GEM install thrift --no-ri --no-rdoc $PROXY
    $SUDO $GEM install puppet --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 1.7.7" json --no-ri --no-rdoc $PROXY
    $SUDO $GEM install rdoc --no-ri --no-rdoc $PROXY
    $SUDO $GEM install gherkin --no-ri --no-rdoc $PROXY
    $SUDO $GEM install cucumber --no-ri --no-rdoc $PROXY
    $SUDO $GEM install capybara --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 1.5.1" rubytter --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 2.2.2" termtter --no-ri --no-rdoc $PROXY
    $SUDO $GEM install userstream --no-ri --no-rdoc $PROXY
    $SUDO $GEM install webtail --no-ri --no-rdoc $PROXY
    $SUDO $GEM install zipruby --no-ri --no-rdoc $PROXY
    $SUDO $GEM install oauth --no-ri --no-rdoc $PROXY
    $SUDO $GEM install webrat --no-ri --no-rdoc $PROXY
    $SUDO $GEM install diff-lcs --no-ri --no-rdoc $PROXY
    $SUDO $GEM install sanitize --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 0.5.8" msgpack --no-ri --no-rdoc $PROXY
    $SUDO $GEM install feed-normalizer --no-ri --no-rdoc $PROXY
    $SUDO $GEM install gettext_rails --no-ri --no-rdoc $PROXY
    $SUDO $GEM install gettext_activerecord --no-ri --no-rdoc $PROXY
    $SUDO $GEM install searchlogic --no-ri --no-rdoc $PROXY
    $SUDO $GEM install fastercsv --no-ri --no-rdoc $PROXY
    $SUDO $GEM install log4r --no-ri --no-rdoc $PROXY
    $SUDO $GEM install sinatra --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 3.0.0" redis --no-ri --no-rdoc $PROXY
    $SUDO $GEM install redis-server --no-ri --no-rdoc $PROXY
    $SUDO $GEM install sidekiq -server --no-ri --no-rdoc $PROXY
    $SUDO $GEM install rubytree --no-ri --no-rdoc $PROXY
    $SUDO $GEM install shoulda --no-ri --no-rdoc $PROXY
    $SUDO $GEM install request-log-analyzer --no-ri --no-rdoc $PROXY
    $SUDO $GEM install apache-loggen --no-ri --no-rdoc $PROXY
    $SUDO $GEM install tzinfo --no-ri --no-rdoc $PROXY
    $SUDO $GEM install guard-coffeescript --no-ri --no-rdoc $PROXY
    $SUDO $GEM install rb-fsevent --no-ri --no-rdoc $PROXY
    $SUDO $GEM install iconv --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "= 0.4.2" i18n --no-ri --no-rdoc $PROXY
    $SUDO $GEM install execjs --no-ri --no-rdoc $PROXY
    $SUDO $GEM install therubyracer --no-ri --no-rdoc $PROXY
    $SUDO $GEM install feedbag --no-ri --no-rdoc $PROXY
    $SUDO $GEM install hashie --no-ri --no-rdoc $PROXY
    $SUDO $GEM install gcalapi --no-ri --no-rdoc $PROXY
    $SUDO $GEM install xml-simple --no-ri --no-rdoc $PROXY
    $SUDO $GEM install pocket-ruby --no-ri --no-rdoc $PROXY
    $SUDO $GEM install weather_hacker --no-ri --no-rdoc $PROXY
    $SUDO $GEM install when_exe --no-ri --no-rdoc $PROXY
    $SUDO $GEM install prawn --no-ri --no-rdoc $PROXY
    $SUDO $GEM install gnuplot --no-ri --no-rdoc $PROXY
    $SUDO $GEM install ai4r --no-ri --no-rdoc $PROXY
    $SUDO $GEM install --source http://gems.github.com tomz-liblinear-ruby-swig $PROXY
    $SUDO $GEM install nimbus --no-ri --no-rdoc $PROXY
    $SUDO $GEM install naivebayes --no-ri --no-rdoc $PROXY
    $SUDO $GEM install kmeans --no-ri --no-rdoc $PROXY
    $SUDO $GEM install recommendation --no-ri --no-rdoc $PROXY
    $SUDO $GEM install stdout --no-ri --no-rdoc $PROXY
    $SUDO $GEM install sysadmin --no-ri --no-rdoc $PROXY
    $SUDO $GEM install automatic --no-ri --no-rdoc $PROXY
    $SUDO $GEM install screening --no-ri --no-rdoc $PROXY
    $SUDO $GEM install ctoD --no-ri --no-rdoc $PROXY
    $SUDO $GEM install count_by --no-ri --no-rdoc $PROXY
    $SUDO $GEM install rubypython --no-ri --no-rdoc $PROXY
    $SUDO $GEM install narray --no-ri --no-rdoc $PROXY
    $SUDO $GEM install gsl --no-ri --no-rdoc $PROXY
    $SUDO $GEM install rmagick --no-ri --no-rdoc $PROXY
    $SUDO $GEM install graphviz --no-ri --no-rdoc $PROXY
    $SUDO $GEM install gviz --no-ri --no-rdoc $PROXY
    $SUDO $GEM install wukong --no-ri --no-rdoc $PROXY
    $SUDO $GEM install woothee --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 0.5.4" webhdfs --no-ri --no-rdoc $PROXY
    #$SUDO $GEM install hadoop-papyrus --no-ri --no-rdoc $PROXY
    #$SUDO $GEM install jruby-on-hadoop --no-ri --no-rdoc $PROXY
    $SUDO $GEM install passenger --no-ri --no-rdoc $PROXY
    $SUDO $GEM install -v "~> 0.10.48" fluentd --no-ri --no-rdoc $PROXY
    $SUDO $GEM install fluent-logger --no-ri --no-rdoc $PROXY
    $SUDO $GEM install fluent-plugin-sqlite3 --no-ri --no-rdoc $PROXY
    $SUDO $GEM install fluent-plugin-s3 --no-ri --no-rdoc $PROXY
    $SUDO $GEM install fluent-plugin-mongo --no-ri --no-rdoc $PROXY
    $SUDO $GEM install fluent-plugin-webhdfs --no-ri --no-rdoc $PROXY
    $SUDO $GEM install fluent-plugin-parser --no-ri --no-rdoc $PROXY
    $SUDO $GEM install fluent-plugin-rewrite-tag-filter --no-ri --no-rdoc $PROXY
    $SUDO $GEM install fluent-plugin-flowcounter --no-ri --no-rdoc $PROXY
    $SUDO $GEM install fluent-plugin-growthforecast --no-ri --no-rdoc $PROXY
    $SUDO $GEM install fluent-plugin-datacounter --no-ri --no-rdoc $PROXY
    $SUDO $GEM install fluent-plugin-twitter --no-ri --no-rdoc $PROXY
    $SUDO $GEM install aws-sdk --no-ri --no-rdoc $PROXY
    $SUDO $GEM install travis --no-ri --no-rdoc $PROXY
    test -f /etc/debian_version && $SUDO $GEM install rsruby --no-ri --no-rdoc $PROXY -- --with-R-include=/usr/include/R --with-R-dir=/usr/lib/R
    test -f /etc/redhat-release && $SUDO $GEM install rsruby --no-ri --no-rdoc $PROXY -- --with-R-include=/usr/share/R/include --with-R-dir=/usr/lib/R
    $GEM list --local
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_gem $*

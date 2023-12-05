#!/bin/sh
#
########################################################################
# Install gem packages
#  $1 = ruby path (ex. /usr/local)
#  $2 = no sudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v2.4 2019-01-14
#       Options removed.
#  v2.3 2014-07-08
#       Auto proxy detection.
#  v2.2 2014-01-30
#       No sudo option.
#  v2.1 2013-04-21
#       option as default.
#  v2.0 2012-10-31
#       Add some gems.
#  v1.0 2008-08-15
#       Stable.
########################################################################

install_gem() {
    test -n "$1" && export GEM=$1/bin/gem
    test -n "$1" || export GEM=/opt/ruby/current/bin/gem
    test -n "$2" || SUDO=sudo
    test -n "$2" && SUDO=
    test "$2" = "sudo" && SUDO=sudo
    test -n "$HTTP_PROXY" || PROXY=
    test -n "$HTTP_PROXY" && PROXY="-r -p $HTTP_PROXY"
    export RUBYOPT=rubygems
    $SUDO $GEM update --system $PROXY
    $SUDO $GEM install rails $PROXY
    $SUDO $GEM install rb-readline $PROXY
    $SUDO $GEM install debugger $PROXY
    $SUDO $GEM install pry $PROXY
    $SUDO $GEM install pry-doc $PROXY
    $SUDO $GEM install pry-debugger $PROXY
    $SUDO $GEM install pry-stack_explorer $PROXY
    $SUDO $GEM install pry-rails $PROXY
    $SUDO $GEM install pry-coolline $PROXY
    $SUDO $GEM install hirb $PROXY
    $SUDO $GEM install hirb-unicode $PROXY
    $SUDO $GEM install awesome_print $PROXY
    $SUDO $GEM install rake $PROXY
    $SUDO $GEM install bundler $PROXY
    $SUDO $GEM install builder $PROXY
    $SUDO $GEM install sqlite3 $PROXY
    $SUDO $GEM install sqlite3-ruby $PROXY
    $SUDO $GEM install sass $PROXY
    $SUDO $GEM install sass-rails $PROXY
    $SUDO $GEM install compass-rails $PROXY
    $SUDO $GEM install coffee-script $PROXY
    $SUDO $GEM install coffee-rails $PROXY
    $SUDO $GEM install jquery-rails $PROXY
    $SUDO $GEM install haml $PROXY
    $SUDO $GEM install haml-rails $PROXY
    $SUDO $GEM install dalli $PROXY
    $SUDO $GEM install backbone-rails $PROXY
    $SUDO $GEM install uglifier $PROXY
    $SUDO $GEM install shotgun $PROXY
    $SUDO $GEM install rails-clean-logs $PROXY
    $SUDO $GEM install devise $PROXY
    $SUDO $GEM install i18n_generators $PROXY
    $SUDO $GEM install jeweler $PROXY
    $SUDO $GEM install gemcutter $PROXY
    $SUDO $GEM install kaminari $PROXY
    $SUDO $GEM install kaminari-bootstrap $PROXY
    $SUDO $GEM install rinku $PROXY
    $SUDO $GEM install yaml_db $PROXY
    #$SUDO $GEM install enumerable-lazy $PROXY
    $SUDO $GEM install term-ansicolor $PROXY
    $SUDO $GEM install turn $PROXY
    $SUDO $GEM install rest-client $PROXY
    $SUDO $GEM install http_configuration $PROXY
    $SUDO $GEM install hpricot $PROXY
    $SUDO $GEM install mechanize $PROXY
    $SUDO $GEM install nokogiri $PROXY
    $SUDO $GEM install anemone $PROXY
    $SUDO $GEM install cosmicrawler $PROXY
    $SUDO $GEM install sanitize $PROXY
    $SUDO $GEM install Selenium $PROXY
    #$SUDO $GEM install pg $PROXY
    #$SUDO $GEM install mysql2 $PROXY
    #$SUDO $GEM install cassandra $PROXY
    $SUDO $GEM install bson $PROXY
    $SUDO $GEM install bson_ext $PROXY
    $SUDO $GEM install mongo $PROXY
    #$SUDO $GEM install BlueCloth $PROXY
    #$SUDO $GEM install RedCloth $PROXY
    $SUDO $GEM install net-ssh $PROXY
    $SUDO $GEM install net-scp $PROXY
    $SUDO $GEM install net-sftp $PROXY
    $SUDO $GEM install net-ping $PROXY
    $SUDO $GEM install coverage $PROXY
    $SUDO $GEM install unicorn $PROXY
    $SUDO $GEM install capistrano $PROXY
    $SUDO $GEM install redgreen $PROXY
    $SUDO $GEM install minitest $PROXY
    $SUDO $GEM install rspec $PROXY
    $SUDO $GEM install rspec-rails $PROXY
    $SUDO $GEM install rspec-mocks $PROXY
    $SUDO $GEM install flexmock $PROXY
    $SUDO $GEM install simplecov $PROXY
    $SUDO $GEM install simplecov-rcov $PROXY
    $SUDO $GEM install sequel $PROXY
    $SUDO $GEM install thrift $PROXY
    $SUDO $GEM install puppet $PROXY
    $SUDO $GEM install json $PROXY
    $SUDO $GEM install rdoc $PROXY
    $SUDO $GEM install gherkin $PROXY
    $SUDO $GEM install cucumber $PROXY
    $SUDO $GEM install capybara $PROXY
    $SUDO $GEM install -v "~> 1.5.1" rubytter $PROXY
    $SUDO $GEM install -v "~> 2.2.9" termtter $PROXY
    $SUDO $GEM install userstream $PROXY
    $SUDO $GEM install webtail $PROXY
    $SUDO $GEM install zipruby $PROXY
    $SUDO $GEM install oauth $PROXY
    $SUDO $GEM install webrat $PROXY
    $SUDO $GEM install diff-lcs $PROXY
    $SUDO $GEM install sanitize $PROXY
    $SUDO $GEM install msgpack $PROXY
    $SUDO $GEM install feed-normalizer $PROXY
    $SUDO $GEM install gettext_rails $PROXY
    $SUDO $GEM install gettext_activerecord $PROXY
    $SUDO $GEM install searchlogic $PROXY
    $SUDO $GEM install fastercsv $PROXY
    $SUDO $GEM install log4r $PROXY
    $SUDO $GEM install foreman $PROXY
    $SUDO $GEM install sinatra $PROXY
    $SUDO $GEM install sinatra_more $PROXY
    $SUDO $GEM install sinatra-formkeeper $PROXY
    $SUDO $GEM install formkeeper-japanese $PROXY
    $SUDO $GEM install redis $PROXY
    $SUDO $GEM install redis-server $PROXY
    $SUDO $GEM install sidekiq -server $PROXY
    $SUDO $GEM install rubytree $PROXY
    $SUDO $GEM install shoulda $PROXY
    $SUDO $GEM install request-log-analyzer $PROXY
    $SUDO $GEM install apache-loggen $PROXY
    $SUDO $GEM install tzinfo $PROXY
    $SUDO $GEM install guard-coffeescript $PROXY
    $SUDO $GEM install rb-fsevent $PROXY
    $SUDO $GEM install iconv $PROXY
    $SUDO $GEM install i18n $PROXY
    $SUDO $GEM install execjs $PROXY
    $SUDO $GEM install therubyracer $PROXY
    $SUDO $GEM install feedbag $PROXY
    $SUDO $GEM install hashie $PROXY
    $SUDO $GEM install gcalapi $PROXY
    $SUDO $GEM install xml-simple $PROXY
    $SUDO $GEM install pocket-ruby $PROXY
    $SUDO $GEM install weather_hacker $PROXY
    $SUDO $GEM install when_exe $PROXY
    $SUDO $GEM install ungarbled $PROXY
    $SUDO $GEM install jpstock $PROXY
    $SUDO $GEM install spreadsheet $PROXY
    $SUDO $GEM install prawn $PROXY
    $SUDO $GEM install pdfkit $PROXY
    $SUDO $GEM install wkhtmltopdf-binary-edge $PROXY
    $SUDO $GEM install gnuplot $PROXY
    $SUDO $GEM install ai4r $PROXY
    $SUDO $GEM install --source http://gems.github.com tomz-liblinear-ruby-swig $PROXY
    $SUDO $GEM install nimbus $PROXY
    $SUDO $GEM install naivebayes $PROXY
    $SUDO $GEM install kmeans $PROXY
    $SUDO $GEM install recommendation $PROXY
    $SUDO $GEM install stdout $PROXY
    $SUDO $GEM install sysadmin $PROXY
    $SUDO $GEM install automatic $PROXY
    $SUDO $GEM install screening $PROXY
    $SUDO $GEM install ctoD $PROXY
    $SUDO $GEM install count_by $PROXY
    $SUDO $GEM install poppler $PROXY
    $SUDO $GEM install rubypython $PROXY
    $SUDO $GEM install narray $PROXY
    $SUDO $GEM install gsl $PROXY
    $SUDO $GEM install rmagick $PROXY
    $SUDO $GEM install graphviz $PROXY
    $SUDO $GEM install gviz $PROXY
    $SUDO $GEM install wukong $PROXY
    $SUDO $GEM install woothee $PROXY
    $SUDO $GEM install webhdfs $PROXY
    #$SUDO $GEM install hadoop-papyrus $PROXY
    #$SUDO $GEM install jruby-on-hadoop $PROXY
    $SUDO $GEM install passenger $PROXY
    $SUDO $GEM install daemons $PROXY
    $SUDO $GEM install eventmachine $PROXY
    $SUDO $GEM install fluentd $PROXY
    $SUDO $GEM install fluent-logger $PROXY
    $SUDO $GEM install fluent-plugin-sqlite3 $PROXY
    $SUDO $GEM install fluent-plugin-s3 $PROXY
    $SUDO $GEM install fluent-plugin-mongo $PROXY
    $SUDO $GEM install fluent-plugin-webhdfs $PROXY
    $SUDO $GEM install fluent-plugin-parser $PROXY
    $SUDO $GEM install fluent-plugin-rewrite-tag-filter $PROXY
    $SUDO $GEM install fluent-plugin-flowcounter $PROXY
    $SUDO $GEM install fluent-plugin-growthforecast $PROXY
    $SUDO $GEM install fluent-plugin-datacounter $PROXY
    $SUDO $GEM install heroku $PROXY
    $SUDO $GEM install aws-sdk $PROXY
    test -f /etc/debian_version && $SUDO $GEM install rsruby $PROXY -- --with-R-include=/usr/include/R --with-R-dir=/usr/lib/R
    test -f /etc/redhat-release && $SUDO $GEM install rsruby $PROXY -- --with-R-include=/usr/share/R/include --with-R-dir=/usr/lib/R
    $GEM list --local
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_gem $*

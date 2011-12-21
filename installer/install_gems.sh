#!/bin/sh
#
########################################################################
# Install gem packages
#  $1 = ruby path (ex. /opt/bin)
#  $2 $3 $4 = proxy
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
# v1.14 12/21,2011
#       Add Guard::CoffeeScript.
# v1.13 6/28,2011
#       Added some gems.
# v1.12 6/2,2011
#       Fix specify the version problem.
# v1.11 5/16,2011
#       Add log4r.
# v1.10 2/24,2011
#       Add sinatra.
#  v1.9 8/20,2010
#       Correspond to termtter 1.9.0 on ruby 1.9.2.
#  v1.8 7/12,2010
#       Freeze version for termtter 1.8.0.
#  v1.7 6/30,2010
#       Refactoring.
#  v1.6 5/7,2010
#       Update to ruby 1.9.
#  v1.5 5/4,2010
#       Add termtter.
#  v1.4 3/7,2010
#       Refactoring.
#  v1.3 8/27,2009
#       Update to rails 2.3.3.
#  v1.2 1/9,2009
#       Add rspec-rails.
#  v1.1 9/16,2008
#       Add rspec.
#  v1.0 8/15,2008
#       Stable.
########################################################################

install_gem() {
    test -n "$1" && export GEM=$1/bin/gem
    test -n "$1" || export GEM=gem
    export RUBYOPT=rubygems
    sudo $GEM install rails $2 $3 $4
    sudo $GEM install sqlite3 $2 $3 $4
    sudo $GEM install sass-rails $2 $3 $4
    sudo $GEM install coffee-rails $2 $3 $4
    sudo $GEM install jquery-rails $2 $3 $4
    sudo $GEM install haml-rails $2 $3 $4
    sudo $GEM install uglifier $2 $3 $4
    sudo $GEM install devise $2 $3 $4
    sudo $GEM install kaminari $2 $3 $4
    sudo $GEM install i18n_generators $2 $3 $4
    #sudo $GEM install mongrel $2 $3 $4
    #sudo $GEM install mongrel_cluster $2 $3 $4
    sudo $GEM install mechanize $2 $3 $4
    sudo $GEM install Selenium $2 $3 $4
    sudo $GEM install vim-ruby $2 $3 $4
    sudo $GEM install postgres-pr $2 $3 $4
    sudo $GEM install mysql $2 $3 $4
    sudo $GEM install BlueCloth $2 $3 $4
    sudo $GEM install RedCloth $2 $3 $4
    sudo $GEM install net-ssh $2 $3 $4
    sudo $GEM install net-sftp $2 $3 $4
    sudo $GEM install coverage $2 $3 $4
    #sudo $GEM install capistrano $2 $3 $4
    #sudo $GEM install magic_multi_connections $2 $3 $4
    #sudo $GEM install redgreen $2 $3 $4
    sudo $GEM install rspec $2 $3 $4
    sudo $GEM install rspec-rails $2 $3 $4
    #sudo $GEM install gherkin $2 $3 $4
    sudo $GEM install cucumber $2 $3 $4
    sudo $GEM install sqlite3-ruby $2 $3 $4
    sudo $GEM install -v 1.4.2 rubytter $2 $3 $4
    sudo $GEM install -v 1.10.0 termtter $2 $3 $4
    sudo $GEM install term-ansicolor $2 $3 $4
    sudo $GEM install nokogiri $2 $3 $4
    #sudo $GEM install webrat $2 $3 $4
    #sudo $GEM install diff-lcs $2 $3 $4
    #sudo $GEM install sanitize $2 $3 $4
    #sudo $GEM install msgpack-rpc $2 $3 $4
    #sudo $GEM install feed-normalizer $2 $3 $4
    #sudo $GEM install will_pagenate $2 $3 $4
    #sudo $GEM install gettext_rails $2 $3 $4
    #sudo $GEM install gettext_activerecord $2 $3 $4
    #sudo $GEM install sanitize $2 $3 $4
    #sudo $GEM install webrat $2 $3 $4
    #sudo $GEM install searchlogic $2 $3 $4
    #sudo $GEM install fastercsv $2 $3 $4
    sudo $GEM install log4r $2 $3 $4
    sudo $GEM install json $2 $3 $4
    #sudo $GEM install sinatra $2 $3 $4
    sudo $GEM install guard-coffeescript $2 $3 $4
    sudo $GEM install -v 0.4.2 i18n $2 $3 $4
    $GEM list --local
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_gem $*

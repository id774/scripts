#!/bin/sh
#
########################################################################
# Install gem packages for ruby 1.8
#  $1 = ruby path (ex. /usr/local)
#  $2 $3 $4 = proxy
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 2013-04-21
#       --no-ri --no-rdoc option as default.
#  v0.2 2012-04-10
#       Update and increase gem packages.
#  v0.1 2012-02-02
#       First.
########################################################################

install_gem() {
    test -n "$1" && export GEM=$1/bin/gem
    test -n "$1" || export GEM=gem
    export RUBYOPT=rubygems

    sudo $GEM install -v 2.3.14 rails --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 0.6.0 i18n --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 2.8.1 mysql --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 3.0.11 passenger --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 1.1.3 bundler --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 1.1.3 rack --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 0.9.2.2 rake --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 3.12 rdoc --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 1.0.6 coderay --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 0.8.2 rubytree --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 1.0.0 daemon_controller --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 0.4.3 edavis10-object_daddy --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 1.5.4 fastercsv --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 1.0.7 fastthread --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 0.10.5 mocha --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 1.6.6 json --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 0.0.1 metaclass --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 0.3.1 net-ldap --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 0.13.2 pg --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 2.13.1 rmagick --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 2.1.8 ruby-openid --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 0.8.2 rubytree --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 3.0.1 shoulda --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 1.0.0 shoulda-context --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 1.0.0 shoulda-matchers --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 1.3.5 sqlite3 --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 1.3.5 sqlite3-ruby --no-ri --no-rdoc $2 $3 $4
    sudo $GEM install -v 0.3.33 tzinfo --no-ri --no-rdoc $2 $3 $4

    sudo $GEM install sqlite3-ruby $2 $3 $4
    $GEM list --local
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
install_gem $*

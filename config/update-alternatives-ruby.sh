#!/bin/sh

########################################################################
# Create environment of "update-alternatives" for ruby
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.3 10/31,2011
#       Add ruby 1.9.3.
#  v1.2 7/21,2010
#       Add ruby 1.8.7.
#  v1.1 5/7,2010
#       Update to ruby 1.9 as default.
#  v1.0 2/21,2010
#       Stable.
########################################################################

remove_alternatives() {
  while [ $# -gt 0 ]
  do
    sudo update-alternatives --remove-all $1
    shift
  done
}

update_alternatives() {
    sudo update-alternatives \
      --install $1/ruby ruby $2/ruby$4 $3\
      --slave   $1/autospec autospec $2/autospec$4\
      --slave   $1/cap cap $2/cap$4\
      --slave   $1/capify capify $2/capify$4\
      --slave   $1/cdiff cdiff $2/cdiff$4\
      --slave   $1/cucumber cucumber $2/cucumber$4\
      --slave   $1/decolor decolor $2/decolor$4\
      --slave   $1/edit_json.rb edit_json.rb $2/edit_json.rb$4\
      --slave   $1/erb erb $2/erb$4\
      --slave   $1/gem gem $2/gem$4\
      --slave   $1/gpgen gpgen $2/gpgen$4\
      --slave   $1/htmldiff htmldiff $2/htmldiff$4\
      --slave   $1/irb irb $2/irb$4\
      --slave   $1/ldiff ldiff $2/ldiff$4\
      --slave   $1/mongrel_rails mongrel_rails $2/mongrel_rails$4\
      --slave   $1/nokogiri nokogiri $2/nokogiri$4\
      --slave   $1/oauth oauth $2/oauth$4\
      --slave   $1/prettify_json.rb prettify_json.rb $2/prettify_json.rb$4\
      --slave   $1/rackup rackup $2/rackup$4\
      --slave   $1/rails rails $2/rails$4\
      --slave   $1/rake rake $2/rake$4\
      --slave   $1/rdoc rdoc $2/rdoc$4\
      --slave   $1/bundle bundle $2/bundle$4\
      --slave   $1/redcloth redcloth $2/redcloth$4\
      --slave   $1/rg rg $2/rg$4\
      --slave   $1/ri ri $2/ri$4\
      --slave   $1/selenium selenium $2/selenium$4\
      --slave   $1/spec spec $2/spec$4\
      --slave   $1/termtter termtter $2/termtter$4\
      --slave   $1/testrb testrb $2/testrb$4\
      --slave   $1/tt tt $2/tt$4\
      --slave   $1/update_rubygems update_rubygems $2/update_rubygems$4\
      --slave   $1/vim-ruby-install.rb vim-ruby-install.rb $2/vim-ruby-install.rb$4
}

make_all_alternatives() {
    TARGET=/opt/bin
    SOURCE=/usr/bin
    PRIORITY=90
    SUFFIX=1.8
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
    SOURCE=/usr/local/bin
    PRIORITY=100
    SUFFIX=
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
    SOURCE=/opt/ruby/1.8.7/bin
    PRIORITY=110
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
    SOURCE=/opt/ruby/1.9.1/bin
    PRIORITY=120
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
    SOURCE=/opt/ruby/1.9.2/bin
    PRIORITY=130
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
    SOURCE=/opt/ruby/1.9.3/bin
    PRIORITY=140
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
    SOURCE=/opt/ruby/1.9.1-dev/bin
    PRIORITY=150
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
    SOURCE=/opt/ruby/1.9.2-dev/bin
    PRIORITY=160
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
    SOURCE=/opt/ruby/1.9.3-dev/bin
    PRIORITY=170
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
}

remove_alternatives ruby autospec cap capify cdiff cucumber decolor \
edit_json.rb erb gem gpgen htmldiff irb ldiff mongrel_rails nokogiri \
oauth prettify_json.rb rackup rails rake rdoc redcloth rg ri selenium \
spec termtter testrb tt update_rubygems vim-ruby-install.rb

make_all_alternatives
sudo update-alternatives --config ruby
ruby -v


#!/bin/sh

########################################################################
# Create environment of "update-alternatives" for ruby
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 2/20,2010
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
      --install $1/ruby   ruby   $2/ruby$4 $3\
      --slave   $1/gem    gem    $2/gem$4\
      --slave   $1/irb    irb    $2/irb$4\
      --slave   $1/rake   rake   $2/rake$4\
      --slave   $1/rdoc   rdoc   $2/rdoc$4\
      --slave   $1/erb    erb    $2/erb$4\
      --slave   $1/testrb testrb $2/testrb$4
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
    SOURCE=/opt/ruby/1.9.1/bin
    PRIORITY=150
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
}

remove_alternatives ruby gem irb rake rdoc erb testrb
make_all_alternatives
sudo update-alternatives --config ruby

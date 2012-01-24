#!/bin/sh
#
########################################################################
# Install Ruby and Rails
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 1/24,2012
#       Ruby 1.9.3 clean install.
#  v0.1 7/11,2011
#       First.
########################################################################

test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts

purge_old_modules() {
    test -d /opt/ruby/1.9.2 && \
      sudo rm -rf /opt/ruby/1.9.2
    test -d /opt/ruby/1.9.3 && \
      sudo rm -rf /opt/ruby/1.9.3
    test -d /usr/local/src/ruby/ruby-1.9.2-p290 && \
      sudo rm -rf /usr/local/src/ruby/ruby-1.9.2-p290
    test -d /usr/local/src/ruby/branches/ruby_1_9_2 && \
      sudo rm -rf /usr/local/src/ruby/branches/ruby_1_9_2
    test -d /usr/local/src/ruby/branches/ruby_1_9_3 && \
      sudo rm -rf /usr/local/src/ruby/branches/ruby_1_9_3
}

install_ruby() {
    $SCRIPTS/installer/install_libyaml.sh
    $SCRIPTS/installer/install_ruby.sh 193-0 /opt/ruby/1.9.3
    $SCRIPTS/installer/install_gems.sh /opt/ruby/1.9.3
    $SCRIPTS/installer/install_termtter_plugins.sh
    /opt/ruby/1.9.3/bin/vim-ruby-install.rb
}

operation() {
    test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
    test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts
    purge_old_modules
    install_ruby
}

operation $*

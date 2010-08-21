#!/bin/sh
#
########################################################################
# Install Termtter Plugins
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 8/19,2010
#       Update to ruby 1.9.2.
#  v1.0 7/26,2010
#       Stable.
########################################################################

update_termtter() {
    if [ -d $1 ]; then
        sudo cp -Rv $HOME/local/github/termtter-plugins/* $1/
    fi
}

termtter_updater() {
    update_termtter /usr/local/lib/ruby/gems/$ruby_version/gems/termtter-$termtter_version/lib/plugins
    update_termtter /opt/local/lib/ruby/gems/$ruby_version/gems/termtter-$termtter_version/lib/plugins
    update_termtter /opt/ruby/$ruby_version/lib/ruby/gems/$ruby_lib_version/gems/termtter-$termtter_version/lib/plugins
    update_termtter /opt/ruby/$ruby_version-dev/lib/ruby/gems/$ruby_lib_version/gems/termtter-$termtter_version/lib/plugins
    update_termtter /opt/ruby/$ruby_version/lib/ruby/gems/$ruby_minor_version/gems/termtter-$termtter_version/lib/plugins
    update_termtter /opt/ruby/$ruby_version-dev/lib/ruby/gems/$ruby_minor_version/gems/termtter-$termtter_version/lib/plugins
    update_termtter /opt/local/lib/ruby/gems/$ruby_version/gems/termtter-$termtter_version/lib/plugins
    update_termtter /opt/local/lib/ruby$ruby_minor_version/gems/$ruby_version/gems/termtter-$termtter_version/lib/plugins
}

update_termtters() {
    ruby_lib_version=1.8.7
    ruby_minor_version=1.8
    ruby_version=1.8
    termtter_updater
    ruby_version=1.8.7
    termtter_updater
    ruby_lib_version=1.9.1
    ruby_minor_version=1.9
    ruby_version=1.9.1
    termtter_updater
    ruby_version=1.9.2
    termtter_updater
}

termtters_updater() {
    termtter_version=1.8.0
    update_termtters
    termtter_version=1.9.0
    update_termtters
}

termtters_updater

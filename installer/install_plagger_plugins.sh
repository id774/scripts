#!/bin/sh
#
########################################################################
# Install Plagger Plugins
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 2011-03-06
#       Add private plagger plugins.
#  v0.1 2010-08-01
#       First.
########################################################################

# Install plagger plugin
export plagger_dir=$HOME/tmp

test -d /usr/local/share/perl/5.8.8/Plagger && export plagger_dir=/usr/local/share/perl/5.8.8/Plagger # Ubuntu 8.04
test -d /usr/local/share/perl/5.10.0/Plagger && export plagger_dir=/usr/local/share/perl/5.10.0/Plagger # Debian 5.0
test -d /usr/local/share/perl/5.10.1/Plagger && export plagger_dir=/usr/local/share/perl/5.10.1/Plagger # Ubuntu 10.04
test -d /usr/local/share/perl/Plagger && export plagger_dir=/usr/local/share/perl/Plagger

sudo cp -Rv $SCRIPTS/cron/plagger/assets/plugins/* $plagger_dir/assets/plugins/
sudo cp -Rv $SCRIPTS/cron/plagger/plugins/publish/* $plagger_dir/Plugin/Publish/
sudo cp -Rv $SCRIPTS/cron/plagger/plugins/filter/* $plagger_dir/Plugin/Filter/
sudo cp -Rv $SCRIPTS/cron/plagger/plugins/customfeed/* $plagger_dir/Plugin/CustomFeed/
sudo cp -Rv $SCRIPTS/cron/plagger/plugins/subscription/* $plagger_dir/Plugin/Subscription/

sudo cp -Rv $PRIVATE/cron/plagger/plugins/filter/* $plagger_dir/Plugin/Filter/
sudo cp -Rv $PRIVATE/cron/plagger/assets/plugins/* $plagger_dir/assets/plugins/

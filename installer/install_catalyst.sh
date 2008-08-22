#!/bin/sh
#
########################################################################
# Install Catalyst
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

sudo aptitude -y install libcatalyst-perl
sudo aptitude -y install libcatalyst-modules-perl
sudo aptitude -y install libcatalyst-modules-extra-perl
sudo aptitude -y install libcatalyst-engine-apache-perl
sudo aptitude -y install libcatalyst-view-tt-perl
sudo aptitude -y install libcatalyst-model-cdbi-perl
sudo aptitude -y install libcatalyst-plugin-formvalidato
sudo aptitude -y install libcatalyst-plugin-session-fast


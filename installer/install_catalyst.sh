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

sudo apt-get -y install libcatalyst-perl
sudo apt-get -y install libcatalyst-modules-perl
sudo apt-get -y install libcatalyst-modules-extra-perl
sudo apt-get -y install libcatalyst-engine-apache-perl
sudo apt-get -y install libcatalyst-view-tt-perl
sudo apt-get -y install libcatalyst-model-cdbi-perl
sudo apt-get -y install libcatalyst-plugin-formvalidato
sudo apt-get -y install libcatalyst-plugin-session-fast


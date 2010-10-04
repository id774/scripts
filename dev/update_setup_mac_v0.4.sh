#!/bin/sh
#
# This scripts updates environment of Mac OS X from 0.4 to 0.5
########################################################################

export SCRIPTS=$HOME/scripts

# Deploy dot_files
$SCRIPTS/installer/install_dotfiles.sh

# Deploy dot_emacs
~/local/github/dot_emacs/install_dotemacs.sh /usr/bin/emacs

# Crypt
$SCRIPTS/installer/install_crypt.sh mac 7.0a

# Termtter
$PRIVATE/installer/install_dottermtter.sh
$SCRIPTS/installer/install_termtter_plugins.sh

# Permissions for /src
sudo chown -R root:wheel /usr/local/src

# MacPorts Update
$SCRIPTS/port_upgrade.sh


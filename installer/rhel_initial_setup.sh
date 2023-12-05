#!/bin/sh
#
########################################################################
# Initial Setup Script for RHEL
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.5 2011-09-26
#       Refactoring.
#  v1.4 2011-09-09
#       Remove truecrypt version number.
#  v1.3 2011-07-11
#       Packed off ruby and frameworks.
#  v1.2 2011-06-08
#       Update to RHEL 6.
#  v1.1 2010-08-29
#       Backported from Ubuntu/Debian.
#  v1.0 2008-08-21
#       Stable.
#  v0.3 2008-05-12
#       Backported from Ubuntu.
#  v0.2 2007-09-04
#       Add Optional Application Install.
#  v0.1 2007-08-31
#       First version.
########################################################################

test -n "$SCRIPTS" || export SCRIPTS=$HOME/scripts
test -n "$PRIVATE" || export PRIVATE=$HOME/private/scripts

operation() {
    # Environment
    $SCRIPTS/installer/rhel_env.sh

    # Packages
    $SCRIPTS/installer/rhel_yum.sh

    # Customize
    $SCRIPTS/installer/rhel_batch_installers.sh

    # GUI Desktop
    #$SCRIPTS/installer/rhel_desktop.sh
}

# redhat?
test -f /etc/redhat-release || exit 1

operation $*

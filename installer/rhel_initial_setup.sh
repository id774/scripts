#!/bin/sh
#
########################################################################
# Initial Setup Script for RHEL
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.5 9/26,2011
#       Refactoring.
#  v1.4 9/9,2011
#       Remove truecrypt version number.
#  v1.3 7/11,2011
#       Packed off ruby and frameworks.
#  v1.2 6/8,2011
#       Update to RHEL 6.
#  v1.1 8/29,2010
#       Backported from Ubuntu/Debian.
#  v1.0 8/21,2008
#       Stable.
#  v0.3 5/12,2008
#       Backported from Ubuntu.
#  v0.2 9/4,2007
#       Add Optional Application Install.
#  v0.1 8/31,2007
#       First version.
########################################################################

export SCRIPTS=$HOME/scripts

operation() {
    # Environment
    $SCRIPTS/installer/rhel_env.sh

    # Packages
    $SCRIPTS/installer/rhel_yum.sh

    # Installers
    $SCRIPTS/installer/rhel_installers.sh

    # GUI Desktop
    #$SCRIPTS/installer/rhel_desktop.sh
}

# redhat?
test -f /etc/redhat-release || exit 1

operation

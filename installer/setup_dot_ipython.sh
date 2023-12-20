#!/bin/sh

########################################################################
# setup_ipython.sh: IPython Setup Script
#
# Description:
# This script automates the setup and configuration of the IPython environment.
# It creates IPython profiles, copies necessary startup files, and sets appropriate
# permissions.
#
# Author: id774 (More info: http://id774.net)
# Source Code: https://github.com/id774/scripts
# License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
# Contact: idnanashi@gmail.com
#
# Version History:
#  v1.1 2023-12-20
#       Refactored script for readability and added documentation.
#  v1.0 2014-08-16
#       Initial release for automating IPython setup.
#
# Usage:
# Run this script to initialize and configure your IPython environment.
# This script sets up a default IPython profile and an additional 'nbserver' profile.
# It also copies necessary startup files from a predefined 'SCRIPTS' directory.
# Before running this script, ensure that the 'SCRIPTS' environment variable points
# to your directory containing the IPython startup files.
#
# Note:
# - This script is intended to be used with Zsh.
# - Make sure to back up any existing IPython configuration before running this script.
# - Ensure that the IPython is installed on your system.
# - 'SCRIPTS' environment variable must be correctly set.
#
########################################################################

# Check if SCRIPTS variable is set
if [ -z "$SCRIPTS" ]; then
    echo "Error: SCRIPTS environment variable is not set."
    echo "Please set the SCRIPTS variable to the path of your IPython startup files."
    exit 1
fi

setup_environment() {
    SCRIPTS=$HOME/scripts

    case $OSTYPE in
      *darwin*)
        OPTIONS=-Rv
        ;;
      *)
        OPTIONS=-Rvd
        ;;
    esac
}

init_nbserver() {
    cd
    ipython profile create nbserver
    test -d $HOME/.ipython/profile_nbserver/startup && cp $OPTIONS $SCRIPTS/dot_files/dot_ipython/profile_default/startup/00-init.py $HOME/.ipython/profile_nbserver/startup/
    test -f $HOME/.ipython/profile_nbserver/startup/00-init.py && chmod -x $HOME/.ipython/profile_nbserver/startup/00-init.py
}

copy_dotipython() {
    cd
    test -d $HOME/.ipython && rm -rf $HOME/.ipython
    ipython profile create default
    test -d $HOME/.ipython/profile_default/startup || mkdir -p $HOME/.ipython/profile_default/startup
    cp $OPTIONS $SCRIPTS/dot_files/dot_ipython/profile_default/startup/00-init.py $HOME/.ipython/profile_default/startup/
    test -f $HOME/.ipython/profile_default/startup/00-init.py && chmod -x $HOME/.ipython/profile_default/startup/00-init.py
    cp $OPTIONS $SCRIPTS/dot_files/dot_zshrc $HOME/.zshrc
    cp $OPTIONS $SCRIPTS/dot_files/dot_zshrc_local $HOME/.zshrc_local
}

main() {
    setup_environment $*
    copy_dotipython $*
    init_nbserver $*
}

main $*

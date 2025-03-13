#!/bin/sh

########################################################################
# setup_ipython.sh: IPython Setup Script
#
#  Description:
#  This script automates the setup and configuration of the IPython environment.
#  It creates IPython profiles, copies necessary startup files, and sets appropriate
#  permissions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#   v1.1 2023-12-20
#        Refactored script for readability and added documentation.
#   v1.0 2014-08-16
#        Initial release for automating IPython setup.
#
#  Usage:
#  Run this script to initialize and configure your IPython environment.
#  This script sets up a default IPython profile and an additional 'nbserver' profile.
#  It also copies necessary startup files from a predefined 'SCRIPTS' directory.
#  Before running this script, ensure that the 'SCRIPTS' environment variable points
#  to your directory containing the IPython startup files.
#
#  Note:
#  - This script is intended to be used with Zsh.
#  - Make sure to back up any existing IPython configuration before running this script.
#  - Ensure that the IPython is installed on your system.
#  - 'SCRIPTS' environment variable must be correctly set.
#
########################################################################

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the path of your IPython startup files." >&2
        exit 1
    fi
}

# Check if IPython is installed and get its path
check_ipython() {
    IPYTHON_PATH=$(command -v ipython)

    if [ -z "$IPYTHON_PATH" ]; then
        echo "Error: IPython is not installed." >&2
        exit 1
    else
        echo "IPython found at: $IPYTHON_PATH"
    fi
}

# Setup environment variables based on OSTYPE
setup_environment() {
     case $OSTYPE in
       *darwin*)
         OPTIONS=-Rv
         ;;
       *)
         OPTIONS=-Rvd
         ;;
     esac
}

# Initialize the nbserver IPython profile
init_nbserver() {
    ipython profile create nbserver
    NB_SERVER_DIR="${HOME}/.ipython/profile_nbserver/startup"

    if [ -d "$NB_SERVER_DIR" ]; then
        cp ${OPTIONS} "${SCRIPTS}/dot_files/dot_ipython/profile_default/startup/00-init.py" "$NB_SERVER_DIR"
        [ -f "${NB_SERVER_DIR}/00-init.py" ] && chmod -x "${NB_SERVER_DIR}/00-init.py"
    fi
}

# Copy .ipython configuration and set up default profile
copy_dotipython() {
    IPYTHON_DIR="${HOME}/.ipython"
    [ -d "$IPYTHON_DIR" ] && rm -rf "$IPYTHON_DIR"

    ipython profile create default
    DEFAULT_PROFILE_DIR="${IPYTHON_DIR}/profile_default/startup"

    [ -d "$DEFAULT_PROFILE_DIR" ] || mkdir -p "$DEFAULT_PROFILE_DIR"
    cp ${OPTIONS} "${SCRIPTS}/dot_files/dot_ipython/profile_default/startup/00-init.py" "$DEFAULT_PROFILE_DIR"
    [ -f "${DEFAULT_PROFILE_DIR}/00-init.py" ] && chmod -x "${DEFAULT_PROFILE_DIR}/00-init.py"

    cp ${OPTIONS} "${SCRIPTS}/dot_files/dot_zshrc" "${HOME}/.zshrc"
    cp ${OPTIONS} "${SCRIPTS}/dot_files/dot_zshrc_local" "${HOME}/.zshrc_local"
}

# Main function to orchestrate setup
main() {
    check_scripts
    check_ipython
    setup_environment
    copy_dotipython $*
    init_nbserver $*
}

# Execute main function
main $*

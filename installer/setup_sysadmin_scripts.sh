#!/bin/sh

########################################################################
# setup_sysadmin_scripts.sh: Install System Administration Scripts
#
#  Description:
#  This script installs or uninstalls a collection of system administration scripts.
#  It allows specifying the installation path and supports uninstalling all scripts.
#  The script handles setting file permissions and ownership appropriately.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-02-14
#       Add els command to installation and uninstallation process.
#  v1.2 2025-01-03
#       Added restart-sshd.sh script to the installation and uninstallation processes.
#  v1.1 2024-12-09
#       Added support for apt-upgrade script in installation and uninstallation processes.
#  v1.0 2023-12-23
#       Refactored for POSIX compliance. Replaced Bash-specific syntax
#       with POSIX standard commands and structures. Enhanced portability
#       and compatibility across different UNIX-like systems.
#  v0.8 2023-11-14
#       Add veramount.
#  v0.7 2023-06-23
#       Fix uninstall bug, add md5, remove some obsolete files.
#  v0.6 2014-05-24
#       Using bash, switch arguments, show uninstall messages.
#  v0.5 2011-06-28
#       Uninstall enabled, RHEL scripts.
#  v0.4 2011-05-27
#       Debian only scripts was moved.
#  v0.3 2011-03-25
#       Install dpkg-hold for Debian.
#  v0.2 2010-12-04
#       Fix copy option on OS X.
#  v0.1 2010-11-16
#       First version.
#
#  Usage:
#  Run this script with the desired action (install or uninstall) and the installation
#   $1 = uninstall
#   $2 = install path (ex. /usr/local/sbin)
#  path as arguments. For example:
#    ./install_sysadmin_scripts.sh install /usr/local/sbin
#    ./install_sysadmin_scripts.sh uninstall
#
#  Note:
#  - Ensure that the 'SCRIPTS' environment variable is set to the path containing
#    the administration scripts.
#  - Running the script with 'uninstall' will remove all installed administration scripts.
#
########################################################################

# Check if the user has sudo privileges (password may be required)
if ! sudo -v 2>/dev/null; then
    echo "Error: This script requires sudo privileges. Please run as a user with sudo access."
    exit 1
fi

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "Error: SCRIPTS environment variable is not set."
        echo "Please set the SCRIPTS variable to the path of your IPython startup files."
        exit 1
    fi
}

setup_environment() {
    test -n "$2" && SBIN=$2
    test -n "$2" || SBIN=/usr/local/sbin
    case $OSTYPE in
      *darwin*)
        OPTIONS=-pR
        OWNER=root:wheel
        ;;
      *)
        OPTIONS=-a
        OWNER=root:root
        ;;
    esac
}

uninstall_scripts() {
    while [ $# -gt 0 ]
    do
        if [ -f "$SBIN/$1" ]; then
            echo "Removing: $SBIN/$1"
            sudo rm -f "$SBIN/$1"
        fi
        shift
    done
}

uninstall_sysadmin_scripts() {
    uninstall_scripts \
        md5 \
        chmodtree \
        cltmp \
        copydir \
        namecalc \
        now \
        waitlock \
        swapext \
        els \
        git-follow-origin \
        git-co-remote-branch \
        git-ignore \
        pyck \
        autopyck \
        get_resources \
        apt-upgrade \
        dpkg-hold \
        gpg-import \
        tcmount \
        veramount \
        platex2pdf \
        userlist \
        usershells \
        port-upgrade \
        port-cleanup \
        restart-sshd
}

install_scripts() {
    sudo cp -v $OPTIONS $SCRIPTS/$2 $SBIN/$3
    sudo chmod $1 $SBIN/$3
    sudo chown $OWNER $SBIN/$3
}

setup_scripts() {
    install_scripts 755 md5.py md5
    install_scripts 755 chmodtree.py chmodtree
    install_scripts 755 cltmp.sh cltmp
    install_scripts 755 els.py els
    install_scripts 755 userlist.py userlist
    install_scripts 755 usershells.py usershells
    install_scripts 755 pyck.py pyck
    install_scripts 755 restart-sshd.sh restart-sshd
}

setup_debian_scripts() {
    install_scripts 755 apt-upgrade.sh apt-upgrade
    install_scripts 755 dpkg-hold.sh dpkg-hold
    install_scripts 755 gpg-import.sh gpg-import
    install_scripts 755 tcmount.py tcmount
}

install_sysadmin_scripts() {
    setup_scripts
    test -f /etc/debian_version && setup_debian_scripts
}

setup_sysadmin_scripts() {
    check_scripts
    setup_environment $*
    test -n "$1" && uninstall_sysadmin_scripts
    test -n "$1" || install_sysadmin_scripts
    test "$1" = "install" && install_sysadmin_scripts
}

setup_sysadmin_scripts $*

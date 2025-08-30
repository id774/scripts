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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script with the desired action (install or uninstall) and the installation
#      $1 = install or uninstall
#      $2 = install path (ex. /usr/local/sbin)
#
#  Arguments:
#      $1 = install or uninstall
#      $2 = optional install path (default: /usr/local/sbin)
#
#  path as arguments. For example:
#      ./install_sysadmin_scripts.sh install
#      ./install_sysadmin_scripts.sh install /usr/local/sbin
#      ./install_sysadmin_scripts.sh uninstall
#
#  Notes:
#  - Ensure that the 'SCRIPTS' environment variable is set to the path containing
#    the administration scripts.
#  - Running the script with 'uninstall' will remove all installed administration scripts.
#
#  Version History:
#  v2.0 2025-08-31
#       Add get-serial and get-device command.
#  v1.9 2025-08-02
#       Add -v option to cp for verbose output during installation.
#       Add existence check for source script in install_scripts function.
#  v1.8 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.7 2025-04-27
#       Add critical failure checks to script installation steps.
#  v1.6 2025-04-14
#       Improve argument parsing to only accept explicit 'install' or 'uninstall'.
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.5 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.4 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
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
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access."
        exit 1
    fi
}

# Check if required commands are available and executable
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set."
        echo "Please set the SCRIPTS variable to the path of your system administration scripts."
        exit 1
    fi
}

# Set up environment variables and system-specific options
setup_environment() {
    test -n "$2" && SBIN="$2"
    test -n "$2" || SBIN=/usr/local/sbin

    case "$(uname -s)" in
        Darwin)
            OPTIONS="-vpR"
            OWNER="root:wheel"
            ;;
        *)
            OPTIONS="-av"
            OWNER="root:root"
            ;;
    esac
}

# Uninstall a list of specified scripts
uninstall_scripts() {
    while [ $# -gt 0 ]
    do
        if [ -f "$SBIN/$1" ]; then
            echo "[INFO] Removing: $SBIN/$1"
            if ! sudo rm -f "$SBIN/$1"; then
                echo "[ERROR] Failed to remove $SBIN/$1." >&2
                exit 1
            fi
        fi
        shift
    done
}

# Uninstall all system administration scripts
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
        get-serial \
        get-device \
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

    echo "[INFO] Uninstallation completed."
}

# Install a script with the specified permissions and ownership
install_scripts() {
    if [ $# -lt 3 ]; then
        echo "[ERROR] install_scripts requires 3 arguments: mode, source_file, target_name." >&2
        exit 2
    fi

    if [ ! -f "$SCRIPTS/$2" ]; then
        echo "[ERROR] Source script '$SCRIPTS/$2' not found." >&2
        exit 1
    fi

    if ! sudo cp $OPTIONS "$SCRIPTS/$2" "$SBIN/$3"; then
        echo "[ERROR] Failed to copy $2 to $SBIN/$3." >&2
        exit 1
    fi

    if ! sudo chmod "$1" "$SBIN/$3"; then
        echo "[ERROR] Failed to set permissions on $SBIN/$3." >&2
        exit 1
    fi

    if ! sudo chown "$OWNER" "$SBIN/$3"; then
        echo "[ERROR] Failed to set ownership on $SBIN/$3." >&2
        exit 1
    fi
}

# Install general system administration scripts
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

# Install Debian-specific system administration scripts
setup_debian_scripts() {
    install_scripts 755 apt-upgrade.sh apt-upgrade
    install_scripts 755 get-serial.sh get-serial
    install_scripts 755 get-device.sh get-device
    install_scripts 755 dpkg-hold.sh dpkg-hold
    install_scripts 755 gpg-import.sh gpg-import
    install_scripts 755 tcmount.py tcmount
}

# Install system administration scripts based on the system type
install_sysadmin_scripts() {
    setup_scripts
    test -f /etc/debian_version && setup_debian_scripts
    echo "[INFO] Installation completed."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_scripts
    check_commands sudo cp chmod chown rm uname
    check_sudo
    setup_environment "$@"
    if [ "$1" = "uninstall" ]; then
        uninstall_sysadmin_scripts
    elif [ "$1" = "install" ] || [ -z "$1" ]; then
        install_sysadmin_scripts
    else
        usage
    fi
    return 0
}

# Execute main function
main "$@"
exit $?

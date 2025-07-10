#!/bin/sh

########################################################################
# debian_apt.sh: Bulk Apt Install Script for Debian
#
#  Description:
#  This script automates the installation of various packages on Debian-based systems.
#  It groups packages by category and only installs those that are not already present,
#  making the setup process efficient and tailored to the system's needs.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script directly without explicit 'sudo', as it uses 'sudo' internally for privilege elevation:
#      ./debian_apt.sh
#
#  You may be prompted to enter your password due to 'sudo' commands within the script. Ensure that you
#  trust the script before executing it, as it performs system updates, upgrades, and installs a pre-defined
#  set of packages, including basic tools, system utilities, development tools, editors, and more.
#
#  Notes:
#  - The script is designed for Debian-based systems.
#  - Ensure internet connectivity for package downloads.
#  - Review and modify the package lists within each category function as needed for your setup.
#
#  Error Conditions:
#  The script checks if each package is already installed to prevent unnecessary reinstallation.
#  However, it does not explicitly handle errors such as package unavailability or network issues.
#  These should be resolved based on the output of the apt-get command.
#
#  Version History:
#  v1.5 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.4 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.3 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.2 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.1 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v1.0 2024-03-20
#       Refactored for POSIX compliance and improved modularity.
#  v0.3 2011-10-03
#       Implement smart_apt function.
#  v0.2 2011-09-28
#       Cut off desktop suite.
#  v0.1 2011-06-16
#       Forked from Initial Setup Script.
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
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Check if the system supports apt-get
check_environment() {
    if ! command -v apt-get >/dev/null 2>&1; then
        echo "[ERROR] apt-get is not available on this system. This script requires a Debian-based environment." >&2
        exit 1
    fi
}

# System update and upgrade
apt_upgrade() {
    sudo apt-get update &&
    sudo apt-get -y upgrade &&
    sudo apt-get autoclean &&
    sudo apt-get -y autoremove
}

# Install package if not already installed
smart_apt() {
    for pkg do  # This iterates over all arguments passed to the function
        if [ "$(dpkg-query -W -f='${Status}' "$pkg" 2>/dev/null | grep -c "ok installed")" -eq 0 ]; then
            sudo apt-get -y install "$pkg"
        fi
    done
}

# Basic packages
basic_packages() {
    smart_apt vim w3m lynx curl wget openssh-server ssh rsync build-essential gcc g++ make \
              gdb cgdb valgrind strace ltrace scons libcunit1 libcunit1-dev libgtest-dev \
              libgoogle-perftools-dev doxygen tar zip gzip unzip bzip2 unar p7zip \
              p7zip-full zsh screen
}

# System packages
system_packages() {
    smart_apt rsyslog ntp keychain fail2ban locales nkf mailutils mutt tree sharutils \
              digitools dnsutils ethtool wakeonlan openmpi-bin xdelta autossh sshfs \
              cifs-utils exfat-fuse exfat-utils postfix sysstat dstat anacron clamav \
              chkrootkit lshw jq arp-scan nmap tcpdump iperf wvdial pwgen vrms manpages-ja \
              manpages-ja-dev lm-sensors needrestart acpi hddtemp smartmontools aptitude
}

# Debian developer tools
debian_developer_tools() {
    smart_apt dpkg-dev lintian debhelper equivs cvs-buildpackage dupload fakeroot \
              devscripts debget dh-make libgtk2.0-dev apt-file software-properties-common \
              bittorrent
}

# Editor packages
editor_packages() {
    smart_apt texlive-lang-cjk texlive-latex-base dvipng texinfo emacs ess mew stunnel \
              ca-certificates w3m-el-snapshot w3m-img imagemagick libmagickcore-dev \
              libmagickwand-dev vim vim-runtime colordiff ctags
}

# EXIF tools
exif_tools() {
    smart_apt exif libimage-exiftool-perl jhead
}

# KVM virtualization
kvm() {
    if grep -qE '^flags.*(vmx|svm)' /proc/cpuinfo; then
        smart_apt kvm libvirt-bin python-libvirt qemu
    fi
}

# Xvfb and related packages
xvfb_packages() {
    smart_apt xvfb fluxbox x11vnc
}

# Programming languages and libraries
lang_packages() {
    smart_apt nasm gauche gauche-dev clisp clisp-dev libboost-dev scheme48 cmuscheme48-el \
              gnu-smalltalk scala r-base r-base-dev ghc cabal-install global markdown \
              graphviz graphviz-dev gsl-bin libgsl-dev libpng-dev libpng12-0 libpng16-16 \
              shunit2 pandoc libyaml-dev
}

# Source control management tools
scm_packages() {
    smart_apt subversion mercurial git
}

# Database packages
db_packages() {
    smart_apt memcached
}

# Samba networking
samba_packages() {
    smart_apt samba cifs-utils smbclient
}

# SQLite packages
sqlite_packages() {
    smart_apt sqlite3 libsqlite3-0 libsqlite3-dev
}

# Optional additional packages
optional_packages() {
    smart_apt gnuserv libxml2 libxml2-dev libxslt-dev libxslt1-dev expat \
              libssl-dev libio-socket-ssl-perl libnet-ssleay-perl libtemplate-perl \
              libxml-libxml-perl libcurl4-openssl-dev libapr1-dev libaprutil1-dev \
              libgpcl-dev
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_environment
    check_sudo
    apt_upgrade
    basic_packages
    system_packages
    debian_developer_tools
    editor_packages
    exif_tools
    # kvm  # Uncomment if KVM support is desired and compatible
    # xvfb_packages  # Uncomment if Xvfb support is needed
    lang_packages
    scm_packages
    db_packages
    # samba_packages
    sqlite_packages
    optional_packages
    # Further package groups can be added here as needed

    echo "[INFO] All specified packages have been installed."
    return 0
}

# Execute main function
main "$@"
exit $?

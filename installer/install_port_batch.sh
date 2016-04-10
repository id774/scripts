#!/bin/sh
#
########################################################################
# MacPorts batch installer
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
# v1.11 6/12,2014
#       Add openblas, scalapack.
# v1.10 11/13,2011
#       Add proctools.
#  v1.9 3/28,2011
#       Remove mysql.
#  v1.8 7/14,2010
#       Recover ruby 1.8.
#  v1.7 7/12,2010
#       Remove ruby.
#  v1.6 3/7,2010
#       Refactoring.
#  v1.5 2/21,2010
#       Add emacs-w3m.
#  v1.4 5/18,2009
#       Add PostgreSQL, launchd startup settings.
#  v1.3 1/22,2009
#       Add smartmontools.
#  v1.2 10/28,2008
#       Add MySQL server configuration.
#  v1.1 8/29,2008
#       Add option -d.
#  v1.0 8/15,2008
#       Stable.
########################################################################

main() {
    sudo port -d selfupdate
    sudo port -d install libiconv +enable_cp932fix
    sudo port -d install coreutils
    sudo port -d install findutils
    sudo port -d install proctools
    sudo port -d install wget
    sudo port -d install curl
    sudo port -d install nkf
    sudo port -d install screen
    sudo port -d install zlib
    sudo port -d install openssl
    sudo port -d install subversion
    sudo port -d install -f svk
    sudo port -d install lv
    sudo port -d install chasen
    sudo port -d install sqlite3
    sudo port -d install libxml
    sudo port -d install libxml2
    sudo port -d install expat
    sudo port -d install p7zip
    sudo port -d install bzip2
    sudo port -d install ctags
    sudo port -d install ncurses
    sudo port -d install vim
    sudo port -d install sshfs
    sudo port -d install openblas
    sudo port -d install scalapack
    sudo port -d install smartmontools
    sudo port -d install w3m
    sudo port -d install emacs-w3m
    sudo port -d install pandoc
    port installed
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main

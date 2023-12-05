#!/bin/sh
#
########################################################################
# MacPorts batch installer
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
# v1.11 2014-06-12
#       Add openblas, scalapack.
# v1.10 2011-11-13
#       Add proctools.
#  v1.9 2011-03-28
#       Remove mysql.
#  v1.8 2010-07-14
#       Recover ruby 1.8.
#  v1.7 2010-07-12
#       Remove ruby.
#  v1.6 2010-03-07
#       Refactoring.
#  v1.5 2010-02-21
#       Add emacs-w3m.
#  v1.4 2009-05-18
#       Add PostgreSQL, launchd startup settings.
#  v1.3 2009-01-22
#       Add smartmontools.
#  v1.2 2008-10-28
#       Add MySQL server configuration.
#  v1.1 2008-08-29
#       Add option -d.
#  v1.0 2008-08-15
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

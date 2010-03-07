#!/bin/sh
#
########################################################################
# MacPorts Ruby 1.9 installer
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 3/7,2010
#       Refactoring.
#  v1.0 2/21,2010
#       Stable.
########################################################################

make_symlnk() {
  while [ $# -gt 0 ]
  do
    sudo ln -fs /opt/local/bin/$11.9 /usr/local/bin/$1
    shift
  done
}

setup_symlink() {
    make_symlnk ruby gem irb erb rake rdoc ri testrb
}

remove_symlink() {
  while [ $# -gt 0 ]
  do
    sudo rm /usr/local/bin/$1
    shift
  done
}

erase_symlink() {
    remove_symlink ruby gem irb erb rake rdoc ri testrb
}

install() {
    sudo port -d install ruby19
    setup_symlink
}

activate() {
    sudo port activate ruby19
    setup_symlink
}

uninstall() {
    sudo port -d uninstall ruby19
    erase_symlink
}

deactivate() {
    sudo port deactivate ruby19
    erase_symlink
}

parse_option() {
    case "$1" in
      activate)
        activate
        ;;
      deactivate)
        deactivate
        ;;
      uninstall)
        uninstall
        ;;
      install)
        ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
        install
        ;;
    esac
}

main() {
    test -n "$1" || exit 1
    test -n "$1" && parse_option "$1"
    ruby -v
}

main $*

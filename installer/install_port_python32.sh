#!/bin/sh
#
########################################################################
# MacPorts Python 3.2 installer
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 6/22,2010
#       First.
########################################################################

make_symlink() {
  while [ $# -gt 0 ]
  do
    sudo ln -fs /opt/local/bin/$13.2 /usr/local/bin/$1
    shift
  done
}

make_custom_symlink() {
    sudo ln -fs /opt/local/bin/python3.2-config /usr/local/bin/python-config
    sudo ln -fs /opt/local/bin/2to3-3.2 /usr/local/bin/2to3
    sudo ln -fs /opt/local/bin/easy_install-3.2 /usr/local/bin/easy_install
}

setup_symlink() {
    make_symlink python pythonw pydoc idle
    make_custom_symlink
}

remove_symlink() {
  while [ $# -gt 0 ]
  do
    sudo rm /usr/local/bin/$1
    shift
  done
}

erase_symlink() {
    remove_symlink python pythonw pydoc idle python-config 2to3
    sudo ln -fs /opt/local/bin/python2.5 /usr/local/bin/python
    sudo ln -fs /opt/local/bin/easy_install-2.5 /usr/local/bin/easy_install
}

install() {
    sudo port -d install python32
    setup_symlink
    curl -O http://python-distribute.org/distribute_setup.py
    sudo python distribute_setup.py
    rm distribute_setup.py
}

activate() {
    sudo port activate python32
    setup_symlink
}

uninstall() {
    sudo port -d uninstall python32
    erase_symlink
}

deactivate() {
    sudo port deactivate python32
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
        ping -c 1 id774.net > /dev/null 2>&1 || exit 1
        install
        ;;
    esac
}

main() {
    test -n "$1" || exit 1
    test -n "$1" && parse_option "$1"
    python -V
}

main $*

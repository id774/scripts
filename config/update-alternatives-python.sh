#!/bin/sh

########################################################################
# Create environment of "update-alternatives" for python
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 10/24,2013
#       Re-construction python environment.
#  v1.0 2/21,2010
#       Stable.
########################################################################

remove_alternatives() {
  while [ $# -gt 0 ]
  do
    sudo update-alternatives --remove-all $1
    shift
  done
}

update_alternatives() {
    test -f $1/python        && sudo rm $1/python
    test -f $1/ipython       && sudo rm $1/ipython
    test -f $1/python-config && sudo rm $1/python-config

    sudo update-alternatives \
      --install $1/python        python        $2/python$4 $3\
      --slave   $1/python-config python-config $2/python$4-config\
      --slave   $1/ipython       ipython       $2/ipython$4
}

make_all_alternatives() {
    TARGET=/usr/bin
    SOURCE=/usr/bin
    PRIORITY=100
    SUFFIX=2.7
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
    SOURCE=/usr/bin
    PRIORITY=110
    SUFFIX=3
    update_alternatives $TARGET $SOURCE $PRIORITY $SUFFIX
}

main() {
    remove_alternatives python
    make_all_alternatives
    sudo update-alternatives --config python
    python -V
}

if [ -f /etc/debian_version ]; then
    main
fi

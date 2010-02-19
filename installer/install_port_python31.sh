#!/bin/sh

sudo port -d install python31

make_symblnk() {
  while [ $# -gt 0 ]
  do
    sudo ln -fs /opt/local/bin/$13.1 /usr/local/bin/$1
    shift
  done
}

make_custom_symblink() {
    sudo ln -fs /opt/local/bin/python3.1-config /usr/local/bin/python-config
    sudo ln -fs /opt/local/bin/2to3-3.1 /usr/local/bin/2to3
}

make_symblnk python pythonw pydoc idle
make_custom_symblink

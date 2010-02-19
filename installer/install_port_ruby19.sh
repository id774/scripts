#!/bin/sh

sudo port -d install ruby19

make_symblnk() {
  while [ $# -gt 0 ]
  do
    sudo ln -fs /opt/local/bin/$11.9 /usr/local/bin/$1
    shift
  done
}

make_symblnk ruby gem irb erb rake rdoc ri testrb

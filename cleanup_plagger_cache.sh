#!/bin/sh

plagger_cleanup_tmpdir() {
  while [ $# -gt 0 ]
  do
    sudo rm -rf /home/plagger/.plagger_tmp/tmp$1
    sudo mkdir /home/plagger/.plagger_tmp/tmp$1
    sudo chown -R plagger:plagger /home/plagger/.plagger_tmp/tmp$1
    sudo chmod 750 /home/plagger/.plagger_tmp/tmp$1
    shift
  done 
}

plagger_cleanup_tmpdir 0 1 2 3 4 5 6 7 8 9 10 11 12 13


#!/bin/sh

plagger_cleanup_tmpdir() {
    sudo rm -rf /home/plagger/.plagger_tmp/$1
    sudo mkdir /home/plagger/.plagger_tmp/$1
    sudo chown -R plagger:plagger /home/plagger/.plagger_tmp/$1
    sudo chmod 750 /home/plagger/.plagger_tmp/$1
}

plagger_cleanup_tmpdir tmp0
plagger_cleanup_tmpdir tmp1
plagger_cleanup_tmpdir tmp2
plagger_cleanup_tmpdir tmp3
plagger_cleanup_tmpdir tmp4
plagger_cleanup_tmpdir tmp5
plagger_cleanup_tmpdir tmp6
plagger_cleanup_tmpdir tmp7
plagger_cleanup_tmpdir tmp8
plagger_cleanup_tmpdir tmp9
plagger_cleanup_tmpdir tmp10
plagger_cleanup_tmpdir tmp11


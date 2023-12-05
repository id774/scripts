#!/bin/sh
#
########################################################################
# Setup tune2fs
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.4 2012-06-28
#       Refactoring.
#  v0.3 2012-04-28
#       Refactoring.
#  v0.2 2012-04-28
#       Update for alias of hostname.
#  v0.1 2011-09-26
#       First version.
########################################################################

exec_tune2fs() {
    test -b $1  && sudo tune2fs -i 0 -c 0 -m 1 $1
}

set_sda() {
    i=0
    while [ $i -lt 10 ]
    do
        exec_tune2fs sda$i
        i=`expr $i + 1`
    done
}

set_for_mapper() {
    exec_tune2fs $mapper-root
    exec_tune2fs $mapper-tmp
    exec_tune2fs $mapper-var
    exec_tune2fs $mapper-opt
    exec_tune2fs $mapper-usr
    exec_tune2fs $mapper-home
    exec_tune2fs $mapper-data
    exec_tune2fs $mapper--root
    exec_tune2fs $mapper--tmp
    exec_tune2fs $mapper--var
    exec_tune2fs $mapper--opt
    exec_tune2fs $mapper--usr
    exec_tune2fs $mapper--home
    exec_tune2fs $mapper--data
}

set_lvm_debian() {
    mapper=/dev/mapper/$HOSTNAME_S
    set_for_mapper
}

set_lvm_rhel() {
    mapper=/dev/mapper/vg_$HOSTNAME_S-lv_$HOSTNAME_S
    set_for_mapper
}

set_lvm_custom() {
    mapper=/dev/mapper/lv_$HOSTNAME_S
    set_for_mapper
}

set_lvm_logvol() {
    i=0
    while [ $i -lt 10 ]
    do
        exec_tune2fs /dev/mapper/vg_$HOSTNAME_S-LogVol0$i
        i=`expr $i + 1`
    done
}

setup_tune2fs() {
    HOSTNAME_S=`/bin/hostname -s`
    set_sda
    set_lvm_debian
    set_lvm_rhel
    set_lvm_custom
    set_lvm_logvol
}

setup_tune2fs

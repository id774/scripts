#!/bin/sh
#
########################################################################
# Setup tune2fs
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 4/28,2012
#       Refactoring.
#  v0.2 4/28,2012
#       Update for alias of hostname.
#  v0.1 9/26,2011
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
}

set_lvm_debian() {
    mapper=/dev/mapper/`/bin/hostname -a`
    set_for_mapper
}

set_lvm_rhel() {
    mapper=/dev/mapper/vg_`/bin/hostname -a`-lv_`/bin/hostname -a`
    set_for_mapper
}

set_lvm_custom() {
    mapper=/dev/mapper/lv_`/bin/hostname -a`
    set_for_mapper
}

set_lvm_logvol() {
    i=0
    while [ $i -lt 10 ]
    do
        exec_tune2fs /dev/mapper/vg_`/bin/hostname -a`-LogVol0$i
        i=`expr $i + 1`
    done
}

setup_tune2fs() {
    set_sda
    set_lvm_debian
    set_lvm_rhel
    set_lvm_custom
    set_lvm_logvol
}

setup_tune2fs

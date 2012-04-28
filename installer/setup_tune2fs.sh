#!/bin/sh
#
########################################################################
# Setup tune2fs
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 4/28,2012
#       Update for alias of hostname.
#  v0.1 9/26,2011
#       First version.
########################################################################

setup_tune2fs() {
    test -b /dev/sda0  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda0
    test -b /dev/sda1  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda1
    test -b /dev/sda2  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda2
    test -b /dev/sda3  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda3
    test -b /dev/sda4  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda4
    test -b /dev/sda5  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda5
    test -b /dev/sda6  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda6
    test -b /dev/sda7  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda7
    test -b /dev/sda8  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda8
    test -b /dev/sda9  && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda9
    test -b /dev/sda10 && sudo tune2fs -i 0 -c 0 -m 1 /dev/sda10

    test -b /dev/mapper/`/bin/hostname -a`-root && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname -a`-root
    test -b /dev/mapper/`/bin/hostname -a`-tmp  && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname -a`-tmp
    test -b /dev/mapper/`/bin/hostname -a`-var  && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname -a`-var
    test -b /dev/mapper/`/bin/hostname -a`-opt  && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname -a`-opt
    test -b /dev/mapper/`/bin/hostname -a`-usr  && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname -a`-usr
    test -b /dev/mapper/`/bin/hostname -a`-home && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/`/bin/hostname -a`-home

    test -b /dev/mapper/lv_`/bin/hostname -a`-root && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname -a`-root
    test -b /dev/mapper/lv_`/bin/hostname -a`-tmp  && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname -a`-tmp
    test -b /dev/mapper/lv_`/bin/hostname -a`-var  && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname -a`-var
    test -b /dev/mapper/lv_`/bin/hostname -a`-opt  && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname -a`-opt
    test -b /dev/mapper/lv_`/bin/hostname -a`-usr  && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname -a`-usr
    test -b /dev/mapper/lv_`/bin/hostname -a`-home && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/lv_`/bin/hostname -a`-home

    test -b /dev/mapper/vg_`/bin/hostname -a`-lv_`/bin/hostname -a`-root && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-lv_`/bin/hostname -a`-root
    test -b /dev/mapper/vg_`/bin/hostname -a`-lv_`/bin/hostname -a`-tmp  && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-lv_`/bin/hostname -a`-tmp
    test -b /dev/mapper/vg_`/bin/hostname -a`-lv_`/bin/hostname -a`-var  && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-lv_`/bin/hostname -a`-var
    test -b /dev/mapper/vg_`/bin/hostname -a`-lv_`/bin/hostname -a`-opt  && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-lv_`/bin/hostname -a`-opt
    test -b /dev/mapper/vg_`/bin/hostname -a`-lv_`/bin/hostname -a`-usr  && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-lv_`/bin/hostname -a`-usr
    test -b /dev/mapper/vg_`/bin/hostname -a`-lv_`/bin/hostname -a`-home && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-lv_`/bin/hostname -a`-home

    test -b /dev/mapper/vg_`/bin/hostname -a`-LogVol01 && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-LogVol01
    test -b /dev/mapper/vg_`/bin/hostname -a`-LogVol02 && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-LogVol02
    test -b /dev/mapper/vg_`/bin/hostname -a`-LogVol03 && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-LogVol03
    test -b /dev/mapper/vg_`/bin/hostname -a`-LogVol04 && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-LogVol04
    test -b /dev/mapper/vg_`/bin/hostname -a`-LogVol05 && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-LogVol05
    test -b /dev/mapper/vg_`/bin/hostname -a`-LogVol06 && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-LogVol06
    test -b /dev/mapper/vg_`/bin/hostname -a`-LogVol07 && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-LogVol07
    test -b /dev/mapper/vg_`/bin/hostname -a`-LogVol08 && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-LogVol08
    test -b /dev/mapper/vg_`/bin/hostname -a`-LogVol09 && \
      sudo tune2fs -i 0 -c 0 -m 1 /dev/mapper/vg_`/bin/hostname -a`-LogVol09
}

setup_tune2fs

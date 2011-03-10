#!/bin/sh

export SCRIPTS=$HOME/scripts
wget -c http://cdimage.debian.org/debian-cd/6.0.0/i386/iso-dvd/MD5SUMS
wget -c http://cdimage.debian.org/debian-cd/6.0.0/i386/iso-dvd/debian-6.0.0-i386-DVD-1.iso
wget -c http://cdimage.debian.org/debian-cd/6.0.0/i386/iso-dvd/debian-6.0.0-i386-DVD-2.iso
wget -c http://cdimage.debian.org/debian-cd/6.0.0/i386/iso-dvd/debian-6.0.0-i386-DVD-3.iso
wget -c http://cdimage.debian.org/debian-cd/6.0.0/i386/iso-dvd/debian-6.0.0-i386-DVD-4.iso
wget -c http://cdimage.debian.org/debian-cd/6.0.0/i386/iso-dvd/debian-6.0.0-i386-DVD-5.iso
wget -c http://cdimage.debian.org/debian-cd/6.0.0/i386/iso-dvd/debian-6.0.0-i386-DVD-6.iso
wget -c http://cdimage.debian.org/debian-cd/6.0.0/i386/iso-dvd/debian-6.0.0-i386-DVD-7.iso
wget -c http://cdimage.debian.org/debian-cd/6.0.0/i386/iso-dvd/debian-6.0.0-i386-DVD-8.iso
$SCRIPTS/md5.sh .
cat MD5SUMS

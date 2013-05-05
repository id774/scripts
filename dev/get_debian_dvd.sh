#!/bin/sh

export SCRIPTS=$HOME/scripts
wget -c http://cdimage.debian.org/debian-cd/7.0.0/i386/iso-dvd/MD5SUMS -O MD5SUMS_i386
wget -c http://cdimage.debian.org/debian-cd/7.0.0/i386/iso-dvd/debian-7.0.0-i386-DVD-1.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/i386/iso-dvd/debian-7.0.0-i386-DVD-2.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/i386/iso-dvd/debian-7.0.0-i386-DVD-3.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/i386/iso-dvd/debian-7.0.0-i386-DVD-4.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/i386/iso-dvd/debian-7.0.0-i386-DVD-5.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/i386/iso-dvd/debian-7.0.0-i386-DVD-6.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/i386/iso-dvd/debian-7.0.0-i386-DVD-7.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/i386/iso-dvd/debian-7.0.0-i386-DVD-8.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/i386/iso-dvd/debian-7.0.0-i386-DVD-9.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/i386/iso-dvd/debian-7.0.0-i386-DVD-10.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/amd64/iso-dvd/MD5SUMS -O MD5SUMS_amd64
wget -c http://cdimage.debian.org/debian-cd/7.0.0/amd64/iso-dvd/debian-7.0.0-amd64-DVD-1.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/amd64/iso-dvd/debian-7.0.0-amd64-DVD-2.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/amd64/iso-dvd/debian-7.0.0-amd64-DVD-3.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/amd64/iso-dvd/debian-7.0.0-amd64-DVD-4.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/amd64/iso-dvd/debian-7.0.0-amd64-DVD-5.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/amd64/iso-dvd/debian-7.0.0-amd64-DVD-6.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/amd64/iso-dvd/debian-7.0.0-amd64-DVD-7.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/amd64/iso-dvd/debian-7.0.0-amd64-DVD-8.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/amd64/iso-dvd/debian-7.0.0-amd64-DVD-9.iso
wget -c http://cdimage.debian.org/debian-cd/7.0.0/amd64/iso-dvd/debian-7.0.0-amd64-DVD-10.iso
$SCRIPTS/md5.sh .
cat MD5SUMS_i386
cat MD5SUMS_amd64

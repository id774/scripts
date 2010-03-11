#!/bin/sh

test -f ~/arc/chromium.tar.gz || exit 1
cd
rm -rf ~/.chromium

tar xzvf ~/arc/chromium.tar.gz


#!/bin/sh

test -d ~/arc || exit 1
test -f ~/arc/chromium.tar.gz && rm ~/arc/chromium.tar.gz
cd

tar czvf ~/arc/chromium.tar.gz .config/chromium


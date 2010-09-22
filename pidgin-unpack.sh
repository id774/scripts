#!/bin/sh

test -f ~/arc/pidgin.tar.gz || exit 1
cd
rm -rf ~/.purple
tar xzvf ~/arc/pidgin.tar.gz


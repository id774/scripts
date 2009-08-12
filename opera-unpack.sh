#!/bin/sh

test -f ~/arc/opera.tar.gz || exit 1
cd
rm -rf ~/.opera
tar xzvf ~/arc/opera.tar.gz


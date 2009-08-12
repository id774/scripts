#!/bin/sh

test -d ~/arc || exit 1
test -f ~/arc/opera.tar.gz && rm ~/arc/opera.tar.gz
cd
tar czvf ~/arc/opera.tar.gz .opera


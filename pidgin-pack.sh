#!/bin/sh

test -d ~/arc || exit 1
test -f ~/arc/pidgin.tar.gz && rm ~/arc/pidgin.tar.gz
cd
tar czvf ~/arc/pidgin.tar.gz .purple
chmod 600 ~/arc/pidgin.tar.gz


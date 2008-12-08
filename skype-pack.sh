#!/bin/sh

test -d ~/arc || exit 1
test -f ~/arc/skype.tar.gz && rm ~/arc/skype.tar.gz
cd
tar czvf ~/arc/skype.tar.gz .Skype


#!/bin/sh

test -f ~/arc/skype.tar.gz || exit 1
cd
rm -rf ~/.Skype
tar xzvf ~/arc/skype.tar.gz


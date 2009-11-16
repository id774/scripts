#!/bin/sh

test -f $SCRIPTS/convert_msime2cannna.rb || exit 1
test -f $SCRIPTS/etc/aa.txt || exit 1
test -d ~/.anthy || mkdir ~/.anthy

ruby -Ku $SCRIPTS/convert_msime2cannna.rb < $SCRIPTS/etc/aa.txt > ~/.anthy/private_words_default
echo "Dictionary created."

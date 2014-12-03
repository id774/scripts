#!/bin/sh

test -n "$2" && gpg --keyserver $1 --recv-keys $2
test -n "$2" && ( sudo gpg --armor --export $2 | sudo apt-key add - )
test -n "$2" || echo "usage: $0 KEYSERVER PUBKEY"


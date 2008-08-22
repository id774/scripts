#!/bin/sh

test -n "$1" && gpg --keyserver pgp.nic.ad.jp --recv-keys $1
test -n "$1" && ( sudo gpg --armor --export $1 | sudo apt-key add - )
test -n "$1" || echo "usage: $0 PUBKEY"


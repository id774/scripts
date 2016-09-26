#!/bin/zsh

test -n "$1" || exit 1

rm -vf $1/**/._*
rm -vf $1/**/.DS_Store
rm -vf $1/**/.*.un\~


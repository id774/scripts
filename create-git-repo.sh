#!/bin/sh

test -n "$1" || exit 1
test -n "$2" && exit 1
mkdir -p /var/lib/git/$1.git
cd /var/lib/git/$1.git
git init --bare --shared
chmod -R o-rwx,g+ws /var/lib/git/$1.git

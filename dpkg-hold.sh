#!/bin/sh

test -n "$2" && echo $1 $2 | sudo dpkg --set-selections && dpkg -l $1
test -n "$2" || ( test -n "$1" && dpkg -l $1 )
test -n "$2" || test -n "$1" || echo "usage: $0 package-name [hold|install]"


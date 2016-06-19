#!/bin/sh

test -n "$1" && USER=$1
test -n "$1" || USER=debian
test -n "$2" && HOST=$2
test -n "$2" || HOST=harpuia

DB_PATH=fastladder/db/fastladder.db

rsync -auvz $USER@$HOST:~/$DB_PATH ~/$DB_PATH

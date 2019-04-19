#!/bin/sh

test -n "$1" && USER=$1
test -n "$1" || USER=debian
test -n "$2" && HOST=$2
test -n "$2" || HOST=harpuia

SOURCE_DB_PATH=fastladder/db/new.db
TARGET_DB_PATH=fastladder/db/fastladder.db

rsync -auvz $USER@$HOST:~/$SOURCE_DB_PATH ~/$TARGET_DB_PATH

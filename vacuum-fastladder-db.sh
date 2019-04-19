#!/bin/sh

cd $HOME/fastladder/db
DB_PATH=fastladder/db/fastladder.db

rm new.db
sqlite3 $HOME/$DB_PATH vacuum
sqlite3 fastladder.db .dump | sqlite3 new.db


#!/bin/sh

USER=debian
HOST=harpuia
DB_PATH=fastladder/db/fastladder.db

sqlite3 ~/$DB_PATH vacuum
rsync -auvz ~/$DB_PATH $USER@$HOST:~/$DB_PATH

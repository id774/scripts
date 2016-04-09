#!/bin/sh

USER=debian
HOST=harpuia
DB_PATH=fastladder/db/fastladder.db

rsync -auvz $USER@$HOST:~/$DB_PATH ~/$DB_PATH

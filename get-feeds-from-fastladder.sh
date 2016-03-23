#!/bin/sh

SQL="select title from feeds;"
COMMAND="sqlite3 -csv -nullvalue 'NULL' $HOME/fastladder/db/fastladder.db"
echo $SQL | $COMMAND

SQL="select count(*) from feeds;"
COMMAND="sqlite3 -csv -nullvalue 'NULL' $HOME/fastladder/db/fastladder.db"
echo $SQL | $COMMAND

exit 0

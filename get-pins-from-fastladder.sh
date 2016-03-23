#!/bin/sh

SQL="select 'fav ' || link from pins;"
COMMAND="sqlite3 -separator , $HOME/fastladder/db/fastladder.db"
echo $SQL | $COMMAND

SQL="delete from pins;"
COMMAND="sqlite3 -separator , $HOME/fastladder/db/fastladder.db"
echo $SQL | $COMMAND

exit 0

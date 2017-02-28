#!/bin/sh

exec_sql() {
    DBFILE="$HOME/fastladder/db/fastladder.db"
    SQL_COMMAND="sqlite3 -separator , $DBFILE"
    echo $* | $SQL_COMMAND
}

SQL="delete from feeds where subscribers_count = 0;"
exec_sql $SQL

SQL="select 'fav ' || link from pins;"
exec_sql $SQL

SQL="delete from pins;"
exec_sql $SQL

exit 0

#!/bin/sh

exec_sql() {
    DBFILE="$HOME/fastladder/db/fastladder.db"
    SQL_COMMAND="sqlite3 -csv -nullvalue 'NULL' $DBFILE"
    echo $* | $SQL_COMMAND
}

SQL="select title from feeds;"
exec_sql $SQL

SQL="select count(*) from feeds;"
exec_sql $SQL

exit 0

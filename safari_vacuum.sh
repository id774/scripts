#!/bin/sh

cd $HOME/Library/Caches/com.apple.Safari
sqlite3 Cache.db vacuum

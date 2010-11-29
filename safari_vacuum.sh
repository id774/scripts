#!/bin/sh

cd ~/Library/Caches/com.apple.Safari
sqlite3 Cache.db vacuum

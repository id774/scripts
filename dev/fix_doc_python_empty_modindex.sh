#!/bin/sh

test -d /var/www/Python-Docs-2.5/ || exit 1
wget http://www.python.jp/doc/2.5/modindex.html
sudo cp modindex.html /var/www/Python-Docs-2.5/
rm modindex.html

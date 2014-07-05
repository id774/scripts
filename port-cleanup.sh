#!/bin/sh

for f in `port installed | grep "@" | grep -v "(active)" | sed -e "s/ //g"`;
do
  sudo port -d -f uninstall $f;
done

#!/bin/sh

for f in `port installed | grep "@" | grep -v "(active)" | sed -e "s/ //g"`;
do
  sudo port -d uninstall $f;
done

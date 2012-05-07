#!/bin/sh

dpkg -l|cut -d ' ' -f3|tail -n +6|xargs apt-cache show|egrep "^Pack|^Supp"|sed -e "s/^Package: /##/g" -e "s/Supported: /,/g"|tr -d '\n'|sed -e "s/##/\n/g"|sort|uniq

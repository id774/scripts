#!/bin/sh

pip install jupyterthemes
pip install --upgrade jupyterthemes
jt -t monokai -f inconsolata -N -T -fs 10 -nfs 10 -ofs 10 -cellw 90% -lineh 140


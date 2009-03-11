#!/bin/sh

test -f /usr/local/bin/vim && sudo rm -v /usr/local/bin/vim
test -f /usr/local/bin/vimtutor && sudo rm -v /usr/local/bin/vimtutor
test -L /usr/local/bin/vimdiff && sudo rm -v /usr/local/bin/vimdiff
test -L /usr/local/bin/view && sudo rm -v /usr/local/bin/view
test -L /usr/local/bin/rvim && sudo rm -v /usr/local/bin/rvim
test -L /usr/local/bin/rview && sudo rm -v /usr/local/bin/rview
test -f /usr/local/bin/xxd && sudo rm -v /usr/local/bin/xxd
test -L /usr/local/bin/ex && sudo rm -v /usr/local/bin/ex
test -d /usr/local/share/vim && sudo rm -vrf /usr/local/share/vim
test -d /usr/local/src/vim && sudo rm -vrf /usr/local/src/vim
sudo rm -vf `find /usr/local/share/man -name 'vim.1'`
sudo rm -vf `find /usr/local/share/man -name 'view.1'`
sudo rm -vf `find /usr/local/share/man -name 'rvim.1'`
sudo rm -vf `find /usr/local/share/man -name 'rview.1'`
sudo rm -vf `find /usr/local/share/man -name 'xxd.1'`
sudo rm -vf `find /usr/local/share/man -name 'ex.1'`
sudo mandb

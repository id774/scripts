#!/bin/bash

sudo apt -y install postfix
cd /etc/munin/plugins/
test -L /etc/munin/plugins/postfix_mailqueue || sudo ln -s /usr/share/munin/plugins/postfix_mailqueue
sudo rm -f *exim*


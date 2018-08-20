#!/bin/sh

cd /lib/x86_64-linux-gnu
sudo ln -s libreadline.so.7.0 libreadline.so.6
cd /usr/bin/
sudo ln -s gpg gpg1


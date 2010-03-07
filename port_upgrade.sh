#!/bin/sh

sudo port -d selfupdate
sudo port -d sync
sudo port -d -u upgrade outdated
port installed


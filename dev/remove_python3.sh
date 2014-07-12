#!/bin/sh

sudo aptitude purge python3.2 python3-minimal python3.2-minimal
sudo update-alternatives --remove-all python


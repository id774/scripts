#!/bin/sh

rm -rf ~/tmp/*
rm -rf ~/.tmp/*
rm -f ~/hardcopy.*
rm -f ~/wget-log*
test -f ~/.emacs.d/%backup%\~ && rm ~/.emacs.d/%backup%\~
test -f ~/%backup%\~ && rm ~/%backup%\~
rm -rf ~/.emacs.d/tmp/*
rm -rf ~/.emacs.d/backups/*
rm -rf ~/.emacs.d/auto-save-list/.saves*
rm -rf ~/.emacs.d/tramp-auto-save/*


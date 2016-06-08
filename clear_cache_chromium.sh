#!/bin/sh

pkill -9 chromium
test -f ~/.config/chromium/Default/Web\ Data && rm -f ~/.config/chromium/Default/Web\ Data && echo "Cache cleared"

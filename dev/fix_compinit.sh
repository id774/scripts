#!/bin/sh

os=$(uname)

if [ "$os" = "Darwin" ]; then
  sudo chown -R root:wheel /usr/local/Homebrew/completions/zsh/
  sudo chown -R root:wheel /usr/local/share/zsh/
else
  exit 1
fi

exit 0

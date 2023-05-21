#!/bin/zsh

os=$(uname)

if [ "$os" = "Darwin" ]; then
  test -f ~/Applications/.localized && rm -vf ~/Applications/.localized
  test -f ~/Documents/.localized && rm -vf ~/Documents/.localized
  test -f ~/Downloads/.localized && rm -vf ~/Downloads/.localized
  test -f ~/Desktop/.localized && rm -vf ~/Desktop/.localized
  test -f ~/Public/.localized && rm -vf ~/Public/.localized
  test -f ~/Pictures/.localized && rm -vf ~/Pictures/.localized
  test -f ~/Music/.localized && rm -vf ~/Music/.localized
  test -f ~/Movies/.localized && rm -vf ~/Movies/.localized
  test -f ~/Library/.localized && rm -vf ~/Library/.localized
  test -f /Applications/.localized && sudo rm -vf /Applications/.localized
  test -f /Applications/Utilities/.localized && sudo rm -vf /Applications/Utilities/.localized
else
  exit 1
fi

exit 0

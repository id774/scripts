#!/bin/zsh

os=$(uname)

if [ "$os" = "Darwin" ]; then
  test -f ~/Applications/.localized || touch ~/Applications/.localized
  test -f ~/Documents/.localized || touch ~/Documents/.localized
  test -f ~/Downloads/.localized || touch ~/Downloads/.localized
  test -f ~/Desktop/.localized || touch ~/Desktop/.localized
  test -f ~/Public/.localized || touch ~/Public/.localized
  test -f ~/Pictures/.localized || touch ~/Pictures/.localized
  test -f ~/Music/.localized || touch ~/Music/.localized
  test -f ~/Movies/.localized || touch ~/Movies/.localized
  test -f ~/Library/.localized || touch ~/Library/.localized
  test -f /Applications/.localized || sudo touch /Applications/.localized
  test -f /Applications/Utilities/.localized || sudo touch /Applications/Utilities/.localized
else
  exit 1
fi

exit 0

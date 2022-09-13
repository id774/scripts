#!/bin/bash

case $OSTYPE in
  *darwin*)
    sudo launchctl unload -w /Library/LaunchDaemons/ssh2.plist && sudo launchctl load -w /Library/LaunchDaemons/ssh2.plist
    ;;
  *)
    sudo systemctl restart ssh.service
    ;;
esac

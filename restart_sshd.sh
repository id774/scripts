#!/bin/bash

case $OSTYPE in
  *darwin*)
    sudo launchctl unload -w /System/Library/LaunchDaemons/ssh.plist && sudo launchctl load -w /System/Library/LaunchDaemons/ssh.plist
    ;;
  *)
    sudo systemctl restart ssh.service
    ;;
esac

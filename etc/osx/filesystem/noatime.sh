#!/bin/sh

sudo mount -vuwo noatime /
sudo cp -v $SCRIPTS/etc/osx/filesystem/com.my.noatime.plist /Library/LaunchDaemons
sudo chown root:wheel /Library/LaunchDaemons/com.my.noatime.plist

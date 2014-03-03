#!/bin/sh

gsettings_settings() {
  gsettings set $1 $2 $3
  echo "gsettings get $1 $2"
  gsettings get $1 $2
}

gsettings_settings org.gnome.desktop.media-handling automount false
gsettings_settings org.gnome.desktop.media-handling automount-open false
gsettings_settings org.gnome.desktop.media-handling autorun-never true


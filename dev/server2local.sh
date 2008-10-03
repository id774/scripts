#!/bin/sh
#
# Mirroring Remote Server to Local Disk
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 9/13,2008
#       First.
########################################################################
# Usage.
# $ server2local.sh server_source local_target
#
# SERVER_SOURCE is your fileserver mount point.
# LOCAL_TARGET is your local disk data directory.
#
# Anytime you need, use symbolic link on your filesystem.
########################################################################

test -n "$1" && SERVER_SOURCE=$1
test -n "$1" || SERVER_SOURCE=$HOME/mnt/server
test -n "$2" && LOCAL_TARGET=$2
test -n "$2" || LOCAL_TARGET=$HOME/mnt/local

server2local() {
  date "+%Y/%m/%d %T"
  echo "* Executing mirroring $SERVER_SOURCE/$1/$2 -> $LOCAL_TARGET/$1/"
  test -d $SERVER_SOURCE/$1/$2 && test -d $LOCAL_TARGET/$1 && gcp -avu $SERVER_SOURCE/$1/$2 $LOCAL_TARGET/$1/
}

########################################################################
# Listing under this line.
# ex)
# server2local private Photo
# server2local private Music
# server2local private Video
# server2local office Documents
# server2local office Pictures
# server2local office Sources
########################################################################

server2local a b
server2local c d

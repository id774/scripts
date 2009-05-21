#!/bin/sh

md5_universal() {
    if [ -x $SCRIPTS/md5dir.rb ]; then
        $SCRIPTS/md5dir.rb $*
    elif [ -x $SCRIPTS/md5dir.py ]; then
        $SCRIPTS/md5dir.py $*
    else
        case $OSTYPE in
          *darwin*)
            md5 $*
            ;;
          *)
            md5sum $*
            ;;
        esac
    fi
}

md5_universal $*

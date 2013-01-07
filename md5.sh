#!/bin/sh

md5_universal() {
    if [ -x $SCRIPTS/md5dir.py ]; then
        $SCRIPTS/md5dir.py $*
    elif [ -x $SCRIPTS/md5dir.rb ]; then
        $SCRIPTS/md5.rb $*
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

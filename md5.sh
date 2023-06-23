#!/bin/sh

md5_universal() {
    if [ -x $SCRIPTS/md5.py ]; then
        $SCRIPTS/md5.py $*
    elif [ -x $SCRIPTS/md5.rb ]; then
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

#!/bin/sh

setup_environment() {
    SCRIPTS=$HOME/scripts

    case $OSTYPE in
      *darwin*)
        OPTIONS=-Rv
        ;;
      *)
        OPTIONS=-Rvd
        ;;
    esac
}

copy_dotipython() {
    test -d $HOME/.ipython/profile_default/startup || mkdir -p $HOME/.ipython/profile_default/startup
    cp $OPTIONS $SCRIPTS/dot_files/dot_ipython/profile_default/startup/00-init.py $HOME/.ipython/profile_default/startup/
}

main() {
    setup_environment $*
    copy_dotipython $*
}

main $*

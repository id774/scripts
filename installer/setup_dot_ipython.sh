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

init_nbserver() {
    cd
    ipython profile create nbserver
    test -d $HOME/.ipython/profile_nbserver/startup && cp $OPTIONS $SCRIPTS/dot_files/dot_ipython/profile_default/startup/00-init.py $HOME/.ipython/profile_nbserver/startup/
}

copy_dotipython() {
    cd
    test -d $HOME/.ipython && rm -rf $HOME/.ipython
    ipython profile create default
    test -d $HOME/.ipython/profile_default/startup || mkdir -p $HOME/.ipython/profile_default/startup
    cp $OPTIONS $SCRIPTS/dot_files/dot_ipython/profile_default/startup/00-init.py $HOME/.ipython/profile_default/startup/
}

main() {
    setup_environment $*
    copy_dotipython $*
    init_nbserver $*
}

main $*

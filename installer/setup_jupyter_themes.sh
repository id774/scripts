#!/bin/sh
#
########################################################################
# Setup Jupyter Theme
#  $1 = python path (ex. /usr/local)
#  $2 = no sudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.1 2021-08-06
#       First.
########################################################################

setup_environment() {
    test -n "$1" && export PIP=$1/bin/pip
    test -n "$1" || export PIP=/opt/python/current/bin/pip
    test -n "$1" && export JUPYTER=$1/bin/jupyter
    test -n "$1" || export JUPYTER=/opt/python/current/bin/jupyter
    test -n "$1" && export JT=$1/bin/jt
    test -n "$1" || export JT=/opt/python/current/bin/jt
    test -n "$2" || SUDO='sudo -H'
    test -n "$2" && SUDO=
    test "$2" = "sudo" && SUDO='sudo -H'
    test -n "$HTTP_PROXY" || PROXY=
    test -n "$HTTP_PROXY" && PROXY="--proxy $HTTP_PROXY"
}

main() {
    setup_environment $*
    test -d $HOME/.jupyter || $JUPYTER notebook --generate-config
    $SUDO $PIP install $PROXY -U jupyterthemes
    $JT -t monokai -f inconsolata -N -T -fs 10 -nfs 10 -ofs 10 -cellw 90% -lineh 140

    echo "div.output_area img, div.output_area svg { background: #C6D3DF; }">>$HOME/.jupyter/custom/custom.css
    vim $HOME/.jupyter/custom/custom.css
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*

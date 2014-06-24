#!/bin/sh
#
########################################################################
# Install Python Libraries.
#  $1 = python path (ex. /usr/local)
#  $2 = no sudo
#  $3 = proxy
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.2 2/10,2014
#       Enable proxy.
#  v0.1 2/9,2014
#       First.
########################################################################

setup_environment() {
    test -n "$1" && export EASY_INSTALL=$1/bin/easy_install
    test -n "$1" || export EASY_INSTALL=easy_install
    test -n "$1" && export PIP=$1/bin/pip
    test -n "$1" || export PIP=pip
    test -n "$2" || SUDO=sudo
    test -n "$2" && SUDO=
    test "$2" = "sudo" && SUDO=sudo
    test -n "$3" || PROXY=
    test -n "$3" && PROXY="--proxy $3"
}

install_libs() {
    $SUDO $PIP install $PROXY -U pip
    $SUDO $PIP install $PROXY Cython
    $SUDO $PIP install $PROXY IPython
    $SUDO $PIP install $PROXY docutils
    $SUDO $PIP install $PROXY nose
    $SUDO $PIP install $PROXY simplejson
    $SUDO $PIP install $PROXY msgpack-python
    $SUDO $PIP install $PROXY numpy
    $SUDO $PIP install $PROXY scipy
    $SUDO $PIP install $PROXY scikit-learn
    $SUDO $PIP install $PROXY matplotlib
    $SUDO $PIP install $PROXY pandas
    $SUDO $PIP install $PROXY patsy
    $SUDO $PIP install $PROXY statsmodels
    $SUDO $PIP install $PROXY sympy
    $SUDO $PIP install $PROXY seaborn
    $SUDO $PIP install $PROXY bokeh
    $SUDO $PIP install $PROXY twisted
    $SUDO $PIP install $PROXY Flask
    $SUDO $PIP install $PROXY django
    $SUDO $PIP install $PROXY SQLAlchemy
    $SUDO $PIP install $PROXY migrate
    $SUDO $PIP install $PROXY readline
    $SUDO $PIP install $PROXY Pygments
    $SUDO $PIP install $PROXY Babel
    $SUDO $PIP install $PROXY Genshi
    $SUDO $PIP install $PROXY bottle
    $SUDO $PIP install $PROXY cherrypy
    $SUDO $PIP install $PROXY beautifulsoup4
    $SUDO $PIP install $PROXY lxml
    $SUDO $PIP install $PROXY requests
    $SUDO $PIP install $PROXY jinja2 tornado pyzmq
    $SUDO $PIP install $PROXY awscli
    $SUDO $PIP install $PROXY cchardet
}

main() {
    setup_environment $*
    install_libs $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*

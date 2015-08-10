#!/bin/sh
#
########################################################################
# Install Python Libraries.
#  $1 = python path (ex. /usr/local)
#  $2 = no sudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.4 8/12,2014
#       Add pip, pyflakes. Upgrade option as default.
#  v0.3 6/27,2014
#       Auto proxy detection.
#  v0.2 2/10,2014
#       Enable proxy.
#  v0.1 2/9,2014
#       First.
########################################################################

setup_environment() {
    test -n "$1" && export EASY_INSTALL=$1/bin/easy_install
    test -n "$1" || export EASY_INSTALL=easy_install
    test -n "$1" && export PIP=$1/bin/pip
    test -n "$1" || export PIP=/opt/python/current/bin/pip
    test -n "$2" || SUDO='sudo -H'
    test -n "$2" && SUDO=
    test "$2" = "sudo" && SUDO='sudo -H'
    test -n "$HTTP_PROXY" || PROXY=
    test -n "$HTTP_PROXY" && PROXY="--proxy $HTTP_PROXY"
}

install_libs() {
    $SUDO $PIP install $PROXY -U 'pip>=7.1.0'
    $SUDO $PIP install $PROXY -U 'IPython>=3.1.0'
    $SUDO $PIP install $PROXY -U pyflakes
    $SUDO $PIP install $PROXY -U flake8
    $SUDO $PIP install $PROXY -U pytest
    $SUDO $PIP install $PROXY -U pytest-pep8
    $SUDO $PIP install $PROXY -U autopep8
    $SUDO $PIP install $PROXY -U autoflake
    $SUDO $PIP install $PROXY -U Cython
    $SUDO $PIP install $PROXY -U docutils
    $SUDO $PIP install $PROXY -U nose
    $SUDO $PIP install $PROXY -U docopt
    $SUDO $PIP install $PROXY -U simplejson
    $SUDO $PIP install $PROXY -U msgpack-python
    $SUDO $PIP install $PROXY -U 'numpy>=1.9.2'
    $SUDO $PIP install $PROXY -U 'scipy>=0.15.1'
    $SUDO $PIP install $PROXY -U 'scikit-learn>=0.16.1'
    $SUDO $PIP install $PROXY -U 'matplotlib>=1.4.3'
    $SUDO $PIP install $PROXY -U 'pandas>=0.16'
    $SUDO $PIP install $PROXY -U japandas
    $SUDO $PIP install $PROXY -U 'pandas-datareader>=0.1.1'
    $SUDO $PIP install $PROXY -U patsy
    $SUDO $PIP install $PROXY -U 'statsmodels>=0.6.1'
    $SUDO $PIP install $PROXY -U sympy
    $SUDO $PIP install $PROXY seaborn
    $SUDO $PIP install $PROXY bokeh
    $SUDO $PIP install $PROXY -U twisted
    $SUDO $PIP install $PROXY -U Flask
    $SUDO $PIP install $PROXY -U Flask-Assets
    $SUDO $PIP install $PROXY -U Flask-Bootstrap
    $SUDO $PIP install $PROXY -U Hamlish-Jinja
    $SUDO $PIP install $PROXY -U gunicorn
    $SUDO $PIP install $PROXY -U 'django>=1.8'
    $SUDO $PIP install $PROXY -U 'SQLAlchemy>=1.0.2'
    $SUDO $PIP install $PROXY -U migrate
    $SUDO $PIP install $PROXY -U readline
    $SUDO $PIP install $PROXY -U Pygments
    $SUDO $PIP install $PROXY -U Babel
    $SUDO $PIP install $PROXY -U Genshi
    $SUDO $PIP install $PROXY -U bottle
    $SUDO $PIP install $PROXY -U cherrypy
    $SUDO $PIP install $PROXY -U beautifulsoup4
    $SUDO $PIP install $PROXY -U lxml
    $SUDO $PIP install $PROXY -U requests
    $SUDO $PIP install $PROXY -U html5lib
    $SUDO $PIP install $PROXY husl
    $SUDO $PIP install $PROXY -U pillow
    $SUDO $PIP install $PROXY -U ggplot
    $SUDO $PIP install $PROXY -U pyper
    $SUDO $PIP install $PROXY -U jinja2 tornado pyzmq
    $SUDO $PIP install $PROXY -U awscli
    $SUDO $PIP install $PROXY -U cchardet
    $SUDO $PIP install $PROXY -U openpyxl
    $SUDO $PIP install $PROXY -U xlrd
    $SUDO $PIP install $PROXY -U simpy
    $SUDO $PIP install $PROXY -U networkx
    $SUDO $PIP install $PROXY -U pdfminer3k
    $SUDO $PIP install $PROXY -U pybrain
    $SUDO $PIP install $PROXY -U fabric
    $SUDO $PIP install $PROXY -U 'pep8<1.6'
    $SUDO $EASY_INSTALL -U TA-Lib
    $SUDO $EASY_INSTALL -U nltk
}

main() {
    setup_environment $*
    install_libs $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*

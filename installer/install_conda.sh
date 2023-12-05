#!/bin/sh
#
########################################################################
# Install Anaconda Libraries.
#  $1 = python path (ex. /usr/local)
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v0.3 2018-05-10
#       Remove channels conda-forge.
#  v0.2 2018-05-02
#       Append channels conda-forge.
#  v0.1 2018-02-28
#       First.
########################################################################

setup_environment() {
    test -n "$1" && export EASY_INSTALL=$1/bin/easy_install
    test -n "$1" || export EASY_INSTALL=$HOME/local/anaconda3/bin/easy_install
    test -n "$1" && export CONDA=$1/bin/conda
    test -n "$1" || export CONDA=$HOME/local/anaconda3/bin/conda
}

install_libs() {
    $CONDA update -n base -y conda
    $CONDA install -y pip
    $CONDA install -y IPython
    $CONDA install -y jupyter
    $CONDA install -y notebook
    $CONDA install -y pyflakes
    $CONDA install -y flake8
    $CONDA install -y pytest
    $CONDA install -y pytest-pep8
    $CONDA install -y autopep8
    $CONDA install -y autoflake
    $CONDA install -y Cython
    $CONDA install -y docutils
    $CONDA install -y nose
    $CONDA install -y docopt
    $CONDA install -y simplejson
    $CONDA install -y msgpack-python
    $CONDA install -y numpy
    $CONDA install -y scipy
    $CONDA install -y scikit-learn
    $CONDA install -y japandas
    $CONDA install -y pandas-datareader
    $CONDA install -y chainer
    $CONDA install -y joblib
    $CONDA install -y dask
    $CONDA install -y patsy
    $CONDA install -y statsmodels
    $CONDA install -y sympy
    #$CONDA install -y pystan
    $CONDA install -y seaborn
    $CONDA install -y bokeh
    $CONDA install -y twisted
    $CONDA install -y Flask
    $CONDA install -y Flask-Assets
    $CONDA install -y Flask-Bootstrap
    $CONDA install -y Hamlish-Jinja
    $CONDA install -y gunicorn
    $CONDA install -y django
    $CONDA install -y SQLAlchemy
    $CONDA install -y lmdb
    $CONDA install -y migrate
    $CONDA install -y readline
    $CONDA install -y Pygments
    $CONDA install -y Babel
    $CONDA install -y Genshi
    $CONDA install -y bottle
    $CONDA install -y cherrypy
    $CONDA install -y beautifulsoup4
    $CONDA install -y lxml
    $CONDA install -y requests
    $CONDA install -y pysolr
    $CONDA install -y watson-developer-cloud
    $CONDA install -y html5lib
    $CONDA install -y husl
    $CONDA install -y pillow
    $CONDA install -y ggplot
    $CONDA install -y pyper
    $CONDA install -y jinja2 tornado pyzmq
    $CONDA install -y awscli
    $CONDA install -y cchardet
    $CONDA install -y openpyxl
    $CONDA install -y xlrd
    $CONDA install -y simpy
    $CONDA install -y networkx
    $CONDA install -y pdfminer3k
    $CONDA install -y pybrain
    $CONDA install -y uwsgi
    $CONDA install -y pypandoc
    $CONDA install -y zipline
    $CONDA install -y DocumentFeatureSelection
    $CONDA install -y python-tr
    $CONDA install -y mod_wsgi
    $CONDA install -y beaker
    $CONDA install -y python-memcached
    $CONDA install -y psycopg2-binary
    $CONDA install -y mpi4py
    $CONDA install -y keras
    $CONDA install -y tensorflow
    $CONDA install -y matplotlib
    $CONDA install -y pandas
    $CONDA install -y pep8
    $EASY_INSTALL TA-Lib
    $EASY_INSTALL nltk
}

main() {
    setup_environment $*
    install_libs $*
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main $*

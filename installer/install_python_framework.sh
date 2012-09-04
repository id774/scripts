#!/bin/sh
#
########################################################################
# Install Python Library and Web Framework
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.3 8/29,2010
#       Use my Library version.
#  v1.2 3/7,2010
#       Refactoring.
#  v1.1 2/18,2010
#       Update to django 1.1.1.
#  v1.0 8/15,2008
#       Stable.
########################################################################

main() {
    TARGET_PATH=`python -c "from distutils.sysconfig import get_python_lib; print get_python_lib()"`

    # Development tool and package manager
    sudo apt-get -y install python-dev
    sudo apt-get -y install python-doc
    sudo apt-get -y install python-docutils
    sudo apt-get -y install python-setuptools
    sudo apt-get -y install python-pkg-resources
    sudo apt-get -y install python-profiler
    sudo apt-get -y install python-notify
    sudo apt-get -y install vim-python
    sudo apt-get -y install readline-common
    sudo apt-get -y install ipython

    # etc
    $SCRIPTS/show_version.py -i

    # Web Application Framework
    $SCRIPTS/installer/install_django.sh
    sudo apt-get -y install python-cherrypy
    sudo apt-get -y install python-twisted
    sudo apt-get -y install python-nose

    # O/R mapper
    sudo apt-get -y install pysqlite
    sudo apt-get -y install python-sqlobject
    #sudo apt-get -y install python-sqlalchemy
    sudo easy_install SQLAlchemy
    #sudo apt-get -y install python-migrate

    # Template
    sudo apt-get -y install python-kid
    sudo apt-get -y install python-cheetah
    sudo apt-get -y install python-genshi
    sudo apt-get -y install clearsilver-dev
    sudo apt-get -y install python-clearsilver

    # RDBMS Binding
    sudo apt-get -y install python-mysqldb
    sudo apt-get -y install python-psycopg2

    # JSON
    sudo easy_install simplejson

    # Twitter
    sudo easy_install python-twitter

    # Web crawler and HTML/XML parser
    sudo apt-get -y install python-mechanize
    $SCRIPTS/installer/install_beautifulsoup.sh
}

ping -c 1 id774.net > /dev/null 2>&1 || exit 1
main

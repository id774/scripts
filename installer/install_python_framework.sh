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
    # Development tool and package manager
    sudo aptitude -y install python-dev
    sudo aptitude -y install python-doc
    sudo aptitude -y install python-docutils
    sudo aptitude -y install python-setuptools
    sudo aptitude -y install python-profiler
    sudo aptitude -y install python-notify
    sudo aptitude -y install vim-python
    sudo aptitude -y install readline-common
    sudo aptitude -y install ipython

    # Web Application Framework
    $SCRIPTS/installer/install_django.sh
    sudo aptitude -y install python-cherrypy
    sudo aptitude -y install python-twisted
    sudo aptitude -y install python-nose

    # O/R mapper
    sudo aptitude -y install python-sqlobject
    #sudo aptitude -y install python-sqlalchemy
    sudo easy_install SQLAlchemy
    #sudo aptitude -y install python-migrate

    # Template
    sudo aptitude -y install python-kid
    sudo aptitude -y install python-cheetah
    sudo aptitude -y install python-genshi
    sudo aptitude -y install clearsilver-dev
    sudo aptitude -y install python-clearsilver

    # RDBMS Binding
    sudo aptitude -y install python-mysqldb
    sudo aptitude -y install python-psycopg2

    # JSON
    sudo easy_install simplejson

    # Twitter
    sudo easy_install python-twitter

    # Web crawler and HTML/XML parser
    sudo aptitude -y install python-mechanize
    TARGET_PATH=`python -c "from distutils.sysconfig import get_python_lib; print get_python_lib()"`
    sudo cp $SCRIPTS/lib/BeautifulSoup.py $TARGET_PATH/BeautifulSoup.py
}

ping -c 1 -i 3 google.com > /dev/null 2>&1 || exit 1
main

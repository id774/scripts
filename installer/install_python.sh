#!/bin/sh
#
########################################################################
# Install Python Library and Web Framework
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.0 8/15,2008
#       Stable.
########################################################################

TARGET_PATH=/usr/lib/python2.5/site-packages

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
sudo aptitude -y install python-django
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
wget http://www.crummy.com/software/BeautifulSoup/download/BeautifulSoup.py
sudo cp BeautifulSoup.py $TARGET_PATH/BeautifulSoup.py
rm BeautifulSoup.py

# The path Python module
wget http://www.jorendorff.com/articles/python/path/src/path.py
sudo cp path.py $TARGET_PATH/path.py
rm path.py


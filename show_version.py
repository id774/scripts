#!/usr/bin/env python
#
########################################################################
# Show Python module versions
#
# options
# -i ... module install
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.1 6/29,2011
#       Add some packages.
#  v1.0 8/15,2008
#       Stable.
########################################################################

import sys, os
p = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'lib')
if not p in sys.path:
    sys.path.append(p)
try:
    p = os.path.join(os.environ['SCRIPTS'], 'lib')
    if not p in sys.path:
        sys.path.append(p)
except KeyError:
    pass

def show_version():
    import python_module_version
    import pkg_resources
    m = python_module_version.PythonModuleVersion()
    print(m.python_version())
    print(m.django_version())
    print(m.mysqldb_version())
    print(m.twisted_version())
    print(m.mechanize_version())
    print(m.nose_version())
    print(m.sqlalchemy_version())
    print(m.migrate_version())
    print(m.turbogears_version())
    print(m.ipython_version())
    print(m.genshi_version())
    print(m.babel_version())
    print(m.pygments_version())
    print(m.docutils_version())
    print(m.textile_version())
    print(m.webpy_version())
    print(m.bottle_version())
    print(m.cherrypy_version())
    print(m.nltk_version())
    print(m.MeCab_version())
    print(m.numpy_version())
    print(m.scipy_version())

def module_install():
    import os
    os.system('sudo easy_install mechanize')
    os.system('sudo easy_install nose')
    os.system('sudo easy_install -Z SQLAlchemy')
    os.system('sudo easy_install migrate')
    os.system('sudo easy_install IPython')
    os.system('sudo easy_install Genshi')
    os.system('sudo easy_install Babel')
    os.system('sudo easy_install Pygments')
    os.system('sudo easy_install docutils')
    os.system('sudo easy_install uuid')
    os.system('sudo easy_install textile')
    os.system('sudo easy_install web.py')
    os.system('sudo easy install -U bottle')
    os.system('sudo easy_install -U cherrypy')
    os.system('sudo easy_install -U distribute')
    os.system('sudo easy_install pip')
    os.system('sudo pip install -U numpy')
    os.system('sudo pip install -U pyyaml nltk')

def main():
    from optparse import OptionParser
    usage = "usage: %prog [options]"
    parser = OptionParser(usage)
    parser.add_option("-i", "--install", help="module install",
                      action="store_true", dest="install")
    (options, args) = parser.parse_args()
    if options.install:
        module_install()
    else:
        show_version()

if __name__=='__main__':
    main()


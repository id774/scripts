#!/usr/bin/env python
#
########################################################################
# Show Python module versions
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v1.2 2/10,2014
#       Remove install process.
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
    m = python_module_version.PythonModuleVersion()
    m.get_module_version('IPython')
    m.get_module_version('docutils')
    m.get_module_version('nose')
    m.get_module_version('simplejson')
    m.get_module_version('numpy')
    m.get_module_version('scipy')
    m.get_module_version('sklearn')
    m.get_module_version('matplotlib')
    m.get_module_version('pandas')
    m.get_module_version('patsy')
    m.get_module_version('statsmodels')
    m.get_module_version('twisted')
    m.get_module_version('Flask')
    m.get_module_version('django')
    m.get_module_version('sqlalchemy')
    m.get_module_version('migrate')
    m.get_module_version('readline')
    m.get_module_version('pygments')
    m.get_module_version('babel')
    m.get_module_version('genshi')
    m.get_module_version('bottle')
    m.get_module_version('cherrypy')
    m.get_module_version('bs4')
    m.get_module_version('nltk')
    m.get_module_version('MeCab')
    m.get_module_version('CaboCha')

def main():
    show_version()

if __name__=='__main__':
    main()


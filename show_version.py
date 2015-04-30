#!/usr/bin/env python
#
########################################################################
# Show python modules info and version
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v2.1 3/19,2014
#       Show python version.
#  v2.0 2/11,2014
#       Simple version listing.
#       Using -i option for detail info.
#  v1.2 2/10,2014
#       Remove install process.
#  v1.1 6/29,2011
#       Add some packages.
#  v1.0 8/15,2008
#       Stable.
########################################################################

import sys
import imp

class PythonModuleInfo:

    def __init__(self, options):
        self.info = options.info
        self.python = options.python

    def _get_module_info(self, module_name):
        try:
            imp.find_module(module_name)
            help(module_name)
        except ImportError:
            print(module_name, 'was not found.')

    def _get_module_version(self, module_name):
        try:
            filename, pathname, description = imp.find_module(module_name)
            obj = imp.load_module(module_name, filename, pathname, description)
            if hasattr(obj, "__version__"):
                print(module_name, obj.__version__)
            elif hasattr(obj, "VERSION"):
                print(module_name, obj.VERSION)
            else:
                print(module_name, "unknown version")
        except ImportError:
            print(module_name, 'was not found.')

    def get_info(self, module_name):
        if self.info:
            self._get_module_info(module_name)
        else:
            self._get_module_version(module_name)

    def get_python_version(self):
        if self.python:
            python_version = sys.version
            print("Python %(python_version)s" % locals())

def main():
    from optparse import OptionParser
    usage = "usage: %prog [options] file"
    parser = OptionParser(usage)
    parser.add_option("-i", "--info", help="show detail info",
                      action="store_true", dest="info")
    parser.add_option("-p", "--python", help="show python version",
                      action="store_true", dest="python")
    (options, args) = parser.parse_args()

    m = PythonModuleInfo(options)
    m.get_python_version()
    m.get_info('pip')
    m.get_info('IPython')
    m.get_info('pep8')
    m.get_info('pyflakes')
    m.get_info('flake8')
    m.get_info('pytest')
    m.get_info('autopep8')
    m.get_info('autoflake')
    m.get_info('cython')
    m.get_info('docutils')
    m.get_info('nose')
    m.get_info('docopt')
    m.get_info('simplejson')
    m.get_info('msgpack')
    m.get_info('numpy')
    m.get_info('scipy')
    m.get_info('sklearn')
    m.get_info('matplotlib')
    m.get_info('pandas')
    m.get_info('japandas')
    m.get_info('patsy')
    m.get_info('statsmodels')
    m.get_info('sympy')
    m.get_info('seaborn')
    m.get_info('bokeh')
    m.get_info('twisted')
    m.get_info('flask')
    m.get_info('django')
    m.get_info('sqlalchemy')
    m.get_info('migrate')
    m.get_info('readline')
    m.get_info('pygments')
    m.get_info('babel')
    m.get_info('genshi')
    m.get_info('bottle')
    m.get_info('cherrypy')
    m.get_info('bs4')
    m.get_info('lxml')
    m.get_info('requests')
    m.get_info('html5lib')
    m.get_info('ggplot')
    m.get_info('pyper')
    m.get_info('awscli')
    m.get_info('openpyxl')
    m.get_info('simpy')
    m.get_info('networkx')
    m.get_info('talib')
    m.get_info('nltk')
    m.get_info('MeCab')
    m.get_info('CaboCha')

if __name__ == '__main__':
    main()

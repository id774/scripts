#!/usr/bin/env python
#
########################################################################
# Show python modules info and version
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
#  v2.2 9/28,2015
#       Refactoring.
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

    packages = [
        'pip',
        'IPython',
        'pep8',
        'pyflakes',
        'flake8',
        'pytest',
        'autopep8',
        'autoflake',
        'cython',
        'docutils',
        'nose',
        'docopt',
        'simplejson',
        'msgpack',
        'numpy',
        'scipy',
        'sklearn',
        'matplotlib',
        'pandas',
        'japandas',
        'dask',
        'patsy',
        'statsmodels',
        'sympy',
        'seaborn',
        'bokeh',
        'twisted',
        'flask',
        'django',
        'sqlalchemy',
        'migrate',
        'readline',
        'pygments',
        'babel',
        'genshi',
        'bottle',
        'cherrypy',
        'bs4',
        'lxml',
        'requests',
        'html5lib',
        'PIL',
        'ggplot',
        'pyper',
        'awscli',
        'openpyxl',
        'simpy',
        'networkx',
        'pdfminer',
        'pybrain',
        'fabric',
        'talib',
        'nltk',
        'MeCab',
        'CaboCha'
    ]

    [m.get_info(p) for p in packages]

if __name__ == '__main__':
    main()

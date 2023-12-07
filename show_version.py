#!/usr/bin/env python

########################################################################
# show_version.py: Show Python Modules Info and Version
#
#  Description:
#  This script displays detailed information and versions for a list of
#  specified Python modules. It also shows the Python version upon request.
#  It's useful for quickly checking the installed versions of various packages.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.3 2020-11-05
#       Using importlib instead of imp.
#  v2.2 2015-09-28
#       Refactoring.
#  v2.1 2014-03-19
#       Show python version.
#  v2.0 2014-02-11
#       Simple version listing.
#       Using -i option for detailed info.
#  v1.2 2014-02-10
#       Remove install process.
#  v1.1 2011-06-29
#       Add some packages.
#  v1.0 2008-08-15
#       Stable.
#
#  Usage:
#  To display the versions of predefined modules:
#      python show_version.py
#
#  To display detailed information of predefined modules:
#      python show_version.py -i
#
#  To display the Python version:
#      python show_version.py -p
#
########################################################################

import sys
import importlib
import warnings

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
            obj = importlib.import_module(module_name)
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
        'tensorflow',
        'keras',
        'joblib',
        'dask',
        'patsy',
        'statsmodels',
        'sympy',
        'pystan',
        'seaborn',
        'bokeh',
        'twisted',
        'flask',
        'django',
        'sqlalchemy',
        'lmdb',
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
        'pysolr',
        'watson-developer-cloud',
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
        'pypandoc',
        'talib',
        'zipline',
        'nltk',
        'MeCab',
        'CaboCha'
    ]

    warnings.resetwarnings()

    with warnings.catch_warnings():
        warnings.simplefilter('ignore')
        [m.get_info(p) for p in packages]


if __name__ == '__main__':
    main()

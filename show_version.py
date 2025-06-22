#!/usr/bin/env python

########################################################################
# show_version.py: Show Python Modules Info and Version
#
#  Description:
#  This script displays detailed information and versions for a list of
#  specified Python modules. It also shows the Python version upon request.
#  Instead of displaying 'not found' messages immediately, it collects them
#  and displays a summary of missing modules at the end.
#  It's useful for quickly checking the installed versions of various packages.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.5 2024-01-18
#       Refactored function names for clarity.
#       Improved comments for better understanding.
#  v2.4 2024-01-06
#       Added functionality to display a summary of not found modules at the end.
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
#  show_version.py [options] file
#
#  Options:
#    -h, --help    show this help message and exit
#    -i, --info    show detail info
#    -p, --python  show python version
#
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

import importlib
import os
import sys
import warnings


def usage():
    script_path = os.path.abspath(__file__)
    in_usage = False
    with open(script_path, 'r', encoding='utf-8') as f:
        for line in f:
            if line.startswith('#  Usage:'):
                in_usage = True
                print(line[2:].strip())
                continue
            if in_usage:
                if line.startswith('#' * 10):
                    break
                if line.startswith('#'):
                    print(line[2:].strip())
    sys.exit(0)

class PythonModuleInfo:

    def __init__(self, options):
        self.info = options.info
        self.python = options.python
        self.not_found = []

    def display_module_help(self, module_name):
        """ Display help information for the specified module. """
        try:
            module = importlib.import_module(module_name)
            help(module)
        except ImportError:
            self.not_found.append(module_name)

    def get_module_version(self, module_name):
        """ Get and display the version of the specified module. """
        try:
            obj = importlib.import_module(module_name)
            if hasattr(obj, "__version__"):
                print(module_name, obj.__version__)
            elif hasattr(obj, "VERSION"):
                print(module_name, obj.VERSION)
            else:
                print(module_name, "unknown version")
        except ImportError:
            self.not_found.append(module_name)

    def get_info(self, module_name):
        if self.info:
            self.display_module_help(module_name)
        else:
            self.get_module_version(module_name)

    def get_python_version(self):
        if self.python:
            python_version = sys.version
            print("Python %(python_version)s" % locals())

    def show_not_found(self):
        if self.not_found:
            print("\nThese modules were not found:")
            for module in self.not_found:
                print(module)

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
        'pyflakes',
        'flake8',
        'pytest',
        'autopep8',
        'autoflake',
        'cython',
        'docutils',
        'docopt',
        'simplejson',
        'numpy',
        'scipy',
        'sklearn',
        'matplotlib',
        'pandas',
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
        'pygments',
        'babel',
        'genshi',
        'bottle',
        'cherrypy',
        'bs4',
        'lxml',
        'requests',
        'pysolr',
        'html5lib',
        'PIL',
        'pyper',
        'awscli',
        'openpyxl',
        'simpy',
        'networkx',
        'fabric',
        'talib',
        'zipline',
        'instaloader',
        'nltk',
        'MeCab',
        'CaboCha'
    ]

    warnings.resetwarnings()

    with warnings.catch_warnings():
        warnings.simplefilter('ignore')
        for p in packages:
            m.get_info(p)

    m.show_not_found()


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] in ('-h', '--help'):
        usage()

    main()

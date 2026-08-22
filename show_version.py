#!/usr/bin/env python

########################################################################
# show_version.py: Show Python Modules Info and Version
#
#  Description:
#  This script diagnoses a broad catalog of representative Python
#  packages. For each package it checks whether it is installed
#  according to distribution metadata, what version is installed, and
#  whether it can actually be imported by the running interpreter.
#  Installed versions are printed as they are confirmed. Packages that
#  cannot be found are collected into a missing-package summary, and
#  packages whose distribution is present but whose import fails are
#  collected into a separate import-failure summary so a broken
#  installation is never mistaken for a simply absent one. It also
#  shows the Python version upon request.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  show_version.py [options]
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
#  Requirements:
#  - Python Version: 3.1 or later
#
#  Version History:
#  v3.0 2026-08-22
#       Modernized the broad Python package catalog to track the current
#       bulk installer targets, added distribution metadata based version
#       detection when available, retained actual module imports as an
#       environment health check, and distinguished missing packages from
#       import failures.
#  v2.7 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v2.6 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
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
########################################################################

import importlib
import os
import sys
import warnings

try:
    from importlib import metadata as importlib_metadata
except ImportError:
    importlib_metadata = None


# List of (distribution-name, import-name) tuples. A list of tuples is
# used instead of a dict so that Python 3.1 is supported and the
# diagnostic order is guaranteed regardless of interpreter version.
PACKAGES = [
    # Packaging / development
    ('pip', 'pip'),
    ('setuptools', 'setuptools'),
    ('wheel', 'wheel'),
    ('pytest', 'pytest'),
    ('black', 'black'),
    ('flake8', 'flake8'),
    ('pyflakes', 'pyflakes'),
    ('autopep8', 'autopep8'),
    ('autoflake', 'autoflake'),
    ('isort', 'isort'),
    ('mypy', 'mypy'),
    ('Cython', 'Cython'),
    ('docutils', 'docutils'),
    # Interactive / notebook
    ('IPython', 'IPython'),
    ('jupyterlab', 'jupyterlab'),
    ('notebook', 'notebook'),
    # Scientific computing / data analysis
    ('numpy', 'numpy'),
    ('scipy', 'scipy'),
    ('pandas', 'pandas'),
    ('polars', 'polars'),
    ('pyarrow', 'pyarrow'),
    ('dask', 'dask'),
    ('joblib', 'joblib'),
    ('patsy', 'patsy'),
    ('statsmodels', 'statsmodels'),
    ('sympy', 'sympy'),
    ('pystan', 'stan'),
    # Visualization
    ('matplotlib', 'matplotlib'),
    ('seaborn', 'seaborn'),
    ('plotly', 'plotly'),
    ('bokeh', 'bokeh'),
    # Classical machine learning
    ('scikit-learn', 'sklearn'),
    ('xgboost', 'xgboost'),
    ('lightgbm', 'lightgbm'),
    ('catboost', 'catboost'),
    # Deep learning / AI
    ('torch', 'torch'),
    ('tensorflow', 'tensorflow'),
    ('keras', 'keras'),
    ('jax', 'jax'),
    ('transformers', 'transformers'),
    ('datasets', 'datasets'),
    ('huggingface_hub', 'huggingface_hub'),
    ('accelerate', 'accelerate'),
    # NLP
    ('nltk', 'nltk'),
    ('spacy', 'spacy'),
    ('gensim', 'gensim'),
    ('mecab-python3', 'MeCab'),
    # Networking / HTTP / parsing
    ('requests', 'requests'),
    ('httpx', 'httpx'),
    ('aiohttp', 'aiohttp'),
    ('Twisted', 'twisted'),
    ('beautifulsoup4', 'bs4'),
    ('lxml', 'lxml'),
    ('html5lib', 'html5lib'),
    # Web frameworks / applications
    ('Flask', 'flask'),
    ('Django', 'django'),
    ('fastapi', 'fastapi'),
    ('uvicorn', 'uvicorn'),
    ('SQLAlchemy', 'sqlalchemy'),
    ('pydantic', 'pydantic'),
    ('bottle', 'bottle'),
    ('CherryPy', 'cherrypy'),
    # Data / document / image
    ('Pillow', 'PIL'),
    ('openpyxl', 'openpyxl'),
    ('PyYAML', 'yaml'),
    ('python-dateutil', 'dateutil'),
    ('Pygments', 'pygments'),
    ('Babel', 'babel'),
    # Graph / simulation
    ('networkx', 'networkx'),
    ('simpy', 'simpy'),
    # Database / infrastructure / service clients
    ('psycopg', 'psycopg'),
    ('pymongo', 'pymongo'),
    ('redis', 'redis'),
    ('boto3', 'boto3'),
    ('lmdb', 'lmdb'),
    ('pysolr', 'pysolr'),
    ('fabric', 'fabric'),
    # Specialized / user utilities
    ('TA-Lib', 'talib'),
    ('instaloader', 'instaloader'),
]


def usage():
    """ Display the script header as usage information and exit. """
    script_path = os.path.abspath(__file__)
    in_header = False
    try:
        with open(script_path, 'r', encoding='utf-8') as f:
            for line in f:
                if line.strip().startswith('#' * 10):
                    if not in_header:
                        in_header = True
                        continue
                    else:
                        break
                if in_header and line.startswith('#'):
                    if line.startswith('# '):
                        print(line[2:], end='')
                    else:
                        print(line[1:], end='')
    except Exception as e:
        print("Error reading usage information: %s" % str(e), file=sys.stderr)
        sys.exit(1)
    sys.exit(0)

class PythonModuleInfo:

    def __init__(self, options):
        self.info = options.info
        self.python = options.python
        self.not_found = []
        self.import_failed = []

    def get_distribution_version(self, distribution_name):
        """ Return (found, version) using distribution metadata, when available. """
        if importlib_metadata is None:
            return (False, None)
        try:
            return (True, importlib_metadata.version(distribution_name))
        except importlib_metadata.PackageNotFoundError:
            return (False, None)
        except Exception:
            return (False, None)

    def get_module_version(self, module):
        """ Get the version of an already imported module from its attributes. """
        if hasattr(module, "__version__"):
            return str(module.__version__)
        elif hasattr(module, "VERSION"):
            return str(module.VERSION)
        else:
            return "unknown version"

    def display_module_help(self, module):
        """ Display help information for an already imported module. """
        help(module)

    def record_import_failed(self, distribution_name, import_name, exception):
        """ Record a package whose distribution resolved but whose import failed. """
        message = ' '.join(str(exception).split())
        self.import_failed.append(
            (distribution_name, import_name, type(exception).__name__, message))

    def check_package(self, distribution_name, import_name):
        """ Check a single package: distribution metadata, version, and actual import. """
        dist_found, dist_version = self.get_distribution_version(distribution_name)

        try:
            module = importlib.import_module(import_name)
        except ImportError as e:
            if importlib_metadata is not None:
                if dist_found:
                    self.record_import_failed(distribution_name, import_name, e)
                else:
                    top_level = import_name.split('.')[0]
                    missing_name = getattr(e, 'name', None)
                    if missing_name is not None and missing_name != top_level:
                        self.record_import_failed(distribution_name, import_name, e)
                    else:
                        self.not_found.append(distribution_name)
            else:
                self.not_found.append(distribution_name)
            return
        except Exception as e:
            self.record_import_failed(distribution_name, import_name, e)
            return

        if dist_found and dist_version:
            version = dist_version
        else:
            version = self.get_module_version(module)

        print(distribution_name, version)

        if self.info:
            self.display_module_help(module)

    def get_python_version(self):
        if self.python:
            python_version = sys.version
            print("Python %(python_version)s" % locals())

    def show_not_found(self):
        if self.not_found:
            print("\nThese packages were not found:")
            for distribution_name in self.not_found:
                print(distribution_name)

    def show_import_failed(self):
        if self.import_failed:
            print("\nThese packages could not be imported:")
            for distribution_name, import_name, exc_class, message in self.import_failed:
                print("%s: %s: %s" % (distribution_name, exc_class, message))

def main():
    from optparse import OptionParser
    usage = "usage: %prog [options]"
    parser = OptionParser(usage)
    parser.add_option("-i", "--info", help="show detail info",
                      action="store_true", dest="info")
    parser.add_option("-p", "--python", help="show python version",
                      action="store_true", dest="python")
    (options, args) = parser.parse_args()

    m = PythonModuleInfo(options)
    m.get_python_version()

    with warnings.catch_warnings():
        warnings.simplefilter('ignore')
        for distribution_name, import_name in PACKAGES:
            m.check_package(distribution_name, import_name)

    m.show_not_found()
    m.show_import_failed()

    return 0


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    sys.exit(main())

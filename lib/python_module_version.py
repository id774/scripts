class PythonModuleVersion:
    def __init__(self):
        pass

    def python_version(self):
        try:
            import sys
            return 'Python', sys.version_info
        except ImportError:
            return 'Python ImportError'

    def django_version(self):
        try:
            import django
            return 'Django', django.VERSION
        except ImportError:
            return 'Django ImportError'

    def mysqldb_version(self):
        try:
            import MySQLdb
            return 'MySQLdb', MySQLdb.version_info
        except ImportError:
            return 'MySQLdb ImportError'

    def twisted_version(self):
        try:
            import twisted
            return 'Twisted', twisted.__version__
        except ImportError:
            return 'Twisted ImportError'

    def mechanize_version(self):
        try:
            import mechanize
            return 'mechanize', mechanize.__version__
        except ImportError:
            return 'mechanize ImportError'

    def nose_version(self):
        try:
            import nose
            return 'nose', nose.__version__
        except ImportError:
            return 'nose ImportError'

    def sqlalchemy_version(self):
        try:
            import sqlalchemy
            return 'SQLAlchemy', sqlalchemy.__version__
        except ImportError:
            return 'SQLAlchemy ImportError'

    def migrate_version(self):
        try:
            import migrate
            return 'migrate Imported'
        except ImportError:
            return 'migrate ImportError'

    def turbogears_version(self):
        try:
            import turbogears
            return 'TurboGears', turbogears.__version__
        except ImportError:
            return 'TurboGears ImportError'

    def ipython_version(self):
        try:
            import IPython
            return 'ipython', IPython.__version__
        except ImportError:
            return 'ipython ImportError'

    def babel_version(self):
        try:
            import babel
            return 'babel', babel.__version__
        except ImportError:
            return 'babel ImportError'

    def pygments_version(self):
        try:
            import pygments
            return 'pygments', pygments.__version__
        except ImportError:
            return 'pygments ImportError'

    def docutils_version(self):
        try:
            import docutils
            return 'docutils', docutils.__version__
        except ImportError:
            return 'docutils ImportError'

    def textile_version(self):
        try:
            import uuid
            import textile
            return 'textile', textile.functions.uuid.__version__
        except ImportError:
            return 'textile ImportError'


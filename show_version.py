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
    print m.python_version()
    print m.django_version()
    print m.mysqldb_version()
    print m.twisted_version()
    print m.mechanize_version()
    print m.nose_version()
    print m.sqlalchemy_version()
    print m.migrate_version()
    print m.turbogears_version()

def module_install():
    import os
    os.system('easy_install mechanize')
    os.system('easy_install nose')
    os.system('easy_install -Z SQLAlchemy')
    os.system('easy_install migrate')

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


#!/usr/bin/env python
#
########################################################################
# Install PostgreSQL
#  usage: install_postgres.py install -g
#
#  install = install postgres
#  -g = install with GUI Tools
#
#  v0.1 2011-05-20
#       First.
########################################################################

import sys
import os

def install_postgres_gui_to_debian():
    syscmd = 'sudo apt-get -y install pgadmin3'
    os.system(syscmd)

def install_postgres_to_debian():
    syscmd = 'sudo apt-get -y install postgresql postgresql-common postgresql-client'
    os.system(syscmd)

def install_postgres(options, args):
    if len(args) > 0 and args[0] == 'install':
        install_postgres_to_debian()
    if options.with_gui_tools:
        install_postgres_gui_to_debian()

def main():
    from optparse import OptionParser
    usage = "usage: %prog [install]"
    parser = OptionParser(usage)
    parser.add_option("-g", "--gui-tools",
                      help="install with GUI Tools",
                      action="store_true", dest="with_gui_tools")
    (options, args) = parser.parse_args()
    install_postgres(options, args)


if __name__ == "__main__":
    main()

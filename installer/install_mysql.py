#!/usr/bin/env python
#
########################################################################
# Install MySQL
#  usage: install_mysql.py install -g -c
#
#  install = install MySQL
#  -g = install with GUI Tools
#  -c = overwrite MySQL Configuration File
#
#  v0.1 2011-05-18
#       First.
########################################################################

import os


def install_mysql_gui_to_debian():
    syscmd = 'sudo apt-get -y install mysql-gui-tools-common'
    os.system(syscmd)

def install_mysql_to_debian():
    syscmd = 'sudo apt-get -y install mysql-server mysql-client'
    os.system(syscmd)

def mysql_configurations():
    syscmd = 'sudo /etc/init.d/mysql stop'
    os.system(syscmd)

    src = os.path.join(os.environ['SCRIPTS'], 'etc', 'my-utf8.cnf')
    dst = os.path.join('/', 'etc', 'mysql', 'my.cnf')
    syscmd = '%scp %s %s' %\
        ('sudo ', src, dst)
    os.system(syscmd)

    syscmd = 'sudo /etc/init.d/mysql start'
    os.system(syscmd)

def install_mysql(options, args):
    if len(args) > 0 and args[0] == 'install':
        install_mysql_to_debian()
    if options.with_gui_tools:
        install_mysql_gui_to_debian()
    if options.configurations:
        mysql_configurations()

def main():
    from optparse import OptionParser
    usage = "usage: %prog [install]"
    parser = OptionParser(usage)
    parser.add_option("-g", "--gui-tools",
                      help="install with GUI Tools",
                      action="store_true", dest="with_gui_tools")
    parser.add_option("-c", "--configurations",
                      help="overwrite MySQL Configuration File",
                      action="store_true", dest="configurations")
    (options, args) = parser.parse_args()
    install_mysql(options, args)


if __name__ == "__main__":
    main()

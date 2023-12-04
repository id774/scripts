#!/usr/bin/env python
#
########################################################################
# chmodtree: Directory Tree chmod Script
#
#  Description:
#  This script changes file and directory permissions recursively within a specified directory.
#  It uses the 'find' command to apply 'chmod' to files and directories matching certain criteria.
#  The script supports options for using 'sudo', controlling verbosity, and filtering by name patterns.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 8/14,2014
#       Minor formatting revisions for readability and consistency.
#  v1.1 1/27,2009
#       Enhanced compatibility to consider platforms of Python other than GNU/Linux.
#  v1.0 1/26,2009
#       Initial release. Basic functionality for recursive chmod in directory trees,
#       with options for sudo, quiet mode, file/directory specificity, and name pattern filtering.
#
# Usage:
#  Run the script with a directory and options:
#      python chmodtree.py [options] dir
#
#  Options include --sudo to execute with superuser privileges, --quiet to reduce output verbosity,
#  --files and --dirs to specify chmod permissions for files and directories, and --name to filter
#  by filename pattern.
#
########################################################################

import sys
import os

def chmodtree(options, arg):
    if options.sudo:
        sudo = 'sudo '
    else:
        sudo = ''

    if options.quiet:
        quiet = ''
    else:
        if sys.platform == 'linux2':
            quiet = '-c '
        else:
            quiet = '-v '

    if options.name:
        name = ' -name "%s"' % options.name
    else:
        name = ''

    if options.files:
        syscmd = '%sfind %s%s -type f -exec chmod %s%s {} \;' %\
            (sudo, arg, name, quiet, options.files)
        os.system(syscmd)

    if options.dirs:
        syscmd = '%sfind %s%s -type d -exec chmod %s%s {} \;' %\
            (sudo, arg, name, quiet, options.dirs)
        os.system(syscmd)

def main():
    from optparse import OptionParser
    usage = "usage: %prog [options] dir"
    parser = OptionParser(usage)
    parser.add_option("-s", "--sudo",
                      help="exec with sudo",
                      action="store_true", dest="sudo")
    parser.add_option("-q", "--quiet",
                      help="shut off non-error messages",
                      action="store_true", dest="quiet")
    parser.add_option("-f", "--files", dest="files",
                      help="chmod files")
    parser.add_option("-d", "--dirs", dest="dirs",
                      help="chmod directory")
    parser.add_option("-n", "--name", dest="name",
                      help="name pattern of find (ex. -n '*.sh')")
    (options, args) = parser.parse_args()
    if len(args) == 1:
        chmodtree(options, args[0])
    else:
        parser.print_help()

if __name__ == "__main__":
    main()

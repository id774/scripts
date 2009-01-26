#!/usr/bin/env python

import sys, os

def chmodtree(options, arg):
    if options.sudo:
        sudo = 'sudo '
    else:
        sudo = ''

    if options.quiet:
        quiet = ''
    else:
        quiet = '-c '

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


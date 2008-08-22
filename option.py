#!/usr/bin/env python

import sys, os
script_dir = os.path.dirname(os.path.abspath(__file__))
python_userlib_dir = script_dir + os.sep + 'lib'
if not python_userlib_dir in sys.path:
    sys.path.append(python_userlib_dir)

def main():
    from optparse import OptionParser
    usage = "usage: %prog [options] arg"
    parser = OptionParser(usage)
    parser.add_option("-f", "--file", dest="filename",
                      help="read data from FILENAME")
    parser.add_option("-v", "--verbose",
                      action="store_true", dest="verbose")
    parser.add_option("-q", "--quiet",
                      action="store_false", dest="verbose")
    (options, args) = parser.parse_args()
    if len(args) != 1:
        parser.error("incorrect number of arguments")
    if options.verbose:
        print "reading %s..." % options.filename
    print options.filename
    print options.verbose
    print args

if __name__ == "__main__":
    main()


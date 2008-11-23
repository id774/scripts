#!/usr/bin/env python

import os, sys
def main():
    from optparse import OptionParser
    usage = "usage: %prog [source_dir] [target_parent_dir]"
    parser = OptionParser(usage)
    parser.add_option("-m", "--mkdir",
                      help="make directory (none as simulate only)",
                      action="store_true", dest="mkdir")
    (options, args) = parser.parse_args()
    if len(args) < 2:
        parser.print_help()
    else:
        for root, dirs, files in os.walk(sys.argv[1]):
            for d in dirs:
                if options.mkdir:
                    os.makedirs(os.path.join(sys.argv[2], root, d))
                    print 'mkdir ' + os.path.join(sys.argv[2], root, d)
                else:
                    print os.path.join(sys.argv[2], root, d)

if __name__=='__main__':
    main()


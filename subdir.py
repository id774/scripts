#!/usr/bin/env python

import os
import sys
def main():
    from optparse import OptionParser
    usage = "usage: %prog [directory]"
    parser = OptionParser(usage)
    parser.add_option("-d", "--delete", help="delete file",
                      action="store_true", dest="delete")
    (options, args) = parser.parse_args()
    if len(args) < 1:
        parser.print_help()
    else:
        for root, dirs, files in os.walk(sys.argv[1]):
            for f in files:
                if options.delete:
                    os.unlink(os.path.join(root, f))
                    print('delete ' + os.path.join(root, f))
                else:
                    print(os.path.join(root, f))

if __name__ == '__main__':
    main()

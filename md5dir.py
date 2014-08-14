#!/usr/bin/env python

import sys
import os
p = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'lib')
if not p in sys.path:
    sys.path.append(p)
try:
    p = os.path.join(os.environ['SCRIPTS'], 'lib')
    if not p in sys.path:
        sys.path.append(p)
except KeyError:
    pass

def md5_dir(subdirectory, args):
    import md5sum
    import os
    from stat import S_ISDIR, ST_MODE
    m = md5sum.Md5sum()
    if subdirectory or S_ISDIR(os.stat(args[0])[ST_MODE]):
        for root, dirs, files in os.walk(args[0]):
            for file in files:
                print((m.get_md5(root + os.sep + file),
                       root + os.sep + file))
    else:
        for f in args:
            print((m.get_md5(f), f))

def main():
    from optparse import OptionParser
    usage = "usage: %prog [options] file"
    parser = OptionParser(usage)
    parser.add_option("-d", "--subdirectory", help="include sub directory",
                      action="store_true", dest="subdirectory")
    (options, args) = parser.parse_args()
    if len(args) < 1:
        parser.print_help()
    else:
        md5_dir(options.subdirectory, args)

if __name__ == '__main__':
    main()

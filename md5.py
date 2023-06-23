#!/usr/bin/env python

import os
import hashlib
from optparse import OptionParser
from stat import S_ISDIR, ST_MODE

class Md5sum:

    @staticmethod
    def get_md5(path):
        m = hashlib.md5()
        with open(path, 'rb') as f:
            for line in f:
                m.update(line)
        return m.hexdigest()

def md5_dir(subdirectory, args):
    if subdirectory or S_ISDIR(os.stat(args[0])[ST_MODE]):
        for root, dirs, files in os.walk(args[0]):
            for file in files:
                print((Md5sum.get_md5(os.path.join(root, file)), 
                       os.path.join(root, file)))
    else:
        for f in args:
            print((Md5sum.get_md5(f), f))

def main():
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


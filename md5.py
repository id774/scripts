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

def md5_dir(subdirectory, reversed, quiet, args):
    if subdirectory and S_ISDIR(os.stat(args[0])[ST_MODE]):
        for root, dirs, files in os.walk(args[0]):
            for file in files:
                if quiet:
                    print(Md5sum.get_md5(os.path.join(root, file)))
                elif reversed:
                    print("{0} {1}".format(
                        Md5sum.get_md5(os.path.join(root, file)),
                        os.path.join(root, file)))
                else:
                    print("MD5 ({0}) = {1}".format(
                        os.path.join(root, file),
                        Md5sum.get_md5(os.path.join(root, file))))
    else:
        for f in args:
            if quiet:
                print(Md5sum.get_md5(f))
            elif reversed:
                print("{0} {1}".format(Md5sum.get_md5(f), f))
            else:
                print("MD5 ({0}) = {1}".format(f, Md5sum.get_md5(f)))

def main():
    usage = "usage: %prog [options] file"
    parser = OptionParser(usage)
    parser.add_option("-d", "--subdirectory", help="Include sub directory.",
                      action="store_true", dest="subdirectory")
    parser.add_option("-r", "--reversed", help="Reverses the format of the output.",
                      action="store_true", dest="reversed")
    parser.add_option("-q", "--quiet", help="Quiet mode - only the checksum is printed out.",
                      action="store_true", dest="quiet")
    (options, args) = parser.parse_args()
    if len(args) < 1:
        parser.print_help()
    else:
        md5_dir(options.subdirectory, options.reversed, options.quiet, args)

if __name__ == '__main__':
    main()


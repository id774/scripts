#!/usr/bin/env python

#!/usr/bin/env python

import os
import hashlib
from optparse import OptionParser
from stat import S_ISDIR, ST_MODE

class Md5Checksum:

    @staticmethod
    def calculate_checksum(path):
        m = hashlib.md5()
        with open(path, 'rb') as f:
            for line in f:
                m.update(line)
        return m.hexdigest()

def print_checksum(include_subdir, reversed_format, quiet_mode, paths):
    for path in paths:
        if include_subdir and S_ISDIR(os.stat(path)[ST_MODE]):
            for root, dirs, files in os.walk(path):
                for file in files:
                    full_path = os.path.join(root, file)
                    checksum = Md5Checksum.calculate_checksum(full_path)
                    print_formatted_checksum(checksum, full_path, reversed_format, quiet_mode)
        else:
            checksum = Md5Checksum.calculate_checksum(path)
            print_formatted_checksum(checksum, path, reversed_format, quiet_mode)

def print_formatted_checksum(checksum, path, reversed_format, quiet_mode):
    if quiet_mode:
        print(checksum)
    elif reversed_format:
        print("{0} {1}".format(checksum, path))
    else:
        print("MD5 ({0}) = {1}".format(path, checksum))

def main():
    usage = "usage: %prog [options] file"
    parser = OptionParser(usage)
    parser.add_option("-d", "--subdirectory", help="Include sub directory.",
                      action="store_true", dest="include_subdir")
    parser.add_option("-r", "--reversed", help="Reverses the format of the output.",
                      action="store_true", dest="reversed_format")
    parser.add_option("-q", "--quiet", help="Quiet mode - only the checksum is printed out.",
                      action="store_true", dest="quiet_mode")
    parser.add_option("-v", "--version", help="Show the version and exit.",
                      action="store_true", dest="version")
    (options, args) = parser.parse_args()
    if options.version:
        import sys
        print("{0} Version 1.0".format(os.path.basename(sys.argv[0])))
    elif len(args) < 1:
        parser.print_help()
    else:
        print_checksum(options.include_subdir, options.reversed_format, options.quiet_mode, args)

if __name__ == '__main__':
    main()


#!/usr/bin/env python

import os, sys
import re
def main():
    from optparse import OptionParser
    usage = "usage: %prog [target_directory] [source_directory]"
    parser = OptionParser(usage)
    (options, args) = parser.parse_args()
    if len(args) < 2:
        parser.print_help()
    else:
        os.chdir(sys.argv[1])
        for root, dirs, files in os.walk(sys.argv[2]):
            for f in files:
                d = re.sub(".zip", "", os.path.basename(f))
                if os.access(d, os.F_OK):
                    pass
                else:
                    os.mkdir(d)
                    os.chdir(d)
                    cmd = ("unzip " + sys.argv[2] + "/" + f) 
                    os.system(cmd)
                    os.chdir("..")

if __name__=='__main__':
    main()


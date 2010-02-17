#!/usr/bin/env python

import os, sys
import re

def expand_directory(args):
    for root, dirs, files in os.walk(args[0]):
        for f in files:
            cmd = ("unzip " + args[0] + os.sep + f) 
            os.system(cmd)

def unzip_subdir(args):
    l = []
    for root, dirs, files in os.walk(args[0]):
        for f in files:
            d = re.sub("\.zip\Z", "", os.path.basename(f))
            if os.access(d, os.F_OK):
                l.append(d)
            else:
                os.mkdir(d)
                os.chdir(d)
                cmd = ("unzip " + args[0] + os.sep + f) 
                os.system(cmd)
                os.chdir("..")
    if len(l) > 0:
        print("These directories already exists.")
        print(l)

def parse_option(options, args):
    os.chdir(args[1])
    if options.expand:
        expand_directory(args)
    else:
        unzip_subdir(args)
                
def main():
    from optparse import OptionParser
    usage = "usage: %prog source_dir target_dir"
    parser = OptionParser(usage)
    parser.add_option("-e", "--expand", help= \
                      "expand directory (not mkdir)",
                      action="store_true", dest="expand")
    (options, args) = parser.parse_args()
    if len(args) < 2:
        parser.print_help()
    else:
        parse_option(options, args)

if __name__=='__main__':
    main()


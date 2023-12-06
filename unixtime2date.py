#!/usr/bin/env python

import sys
import datetime

def unixtime2date(its):
    return datetime.datetime.fromtimestamp(its).strftime('%Y/%m/%d %H:%M:%S')

def main(args):
    print(unixtime2date(int(args[1])))

if __name__ == '__main__':
    argsmin = 1
    version = (3, 0)
    if sys.version_info > (version):
        if len(sys.argv) > argsmin:
            main(sys.argv)
        else:
            print("This program needs at least %(argsmin)s arguments" %
                  locals())
    else:
        print("This program requires python > %(version)s" % locals())

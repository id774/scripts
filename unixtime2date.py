#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import datetime

def main(args):
    its = int(args[1])
    now = datetime.datetime.fromtimestamp(its).strftime('%Y/%m/%d %H:%M:%S')
    print(now)

if __name__ == '__main__':
    argsmin = 1
    version = (3,0)
    if sys.version_info > (version):
        if len(sys.argv) > argsmin:
            main(sys.argv)
        else:
            print("This program needs at least %(argsmin)s arguments" %locals())
    else:
        print("This program requires python > %(version)s" %locals())


#!/usr/bin/env python
# -*- coding: utf-8 -*-

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

def calculate_apache_ip_hits(log):
    from apache_calculater import ApacheCalculater
    return ApacheCalculater.calculateApacheIpHits(log)

def main():
    if len(sys.argv) != 2:
        print("Usage: calculate_apache_hits log_file_name")
        sys.exit(1)

    print(calculate_apache_ip_hits(sys.argv[1]))

if __name__ == '__main__':
    main()

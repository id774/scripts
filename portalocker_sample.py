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

import portalocker

afile = open("portalocker_sample.py")
portalocker.lock(afile, portalocker.LOCK_EX)

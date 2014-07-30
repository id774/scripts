#!/usr/bin/env python
# - * - coding: utf-8 - * -

from nose.tools import *
from unixtime2date import *

def test_unixtime2date():
    its = 1406524320
    e = '2014/07/28 14:12:00'
    eq_(e, unixtime2date(its))


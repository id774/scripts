#!/usr/bin/env python

import string
from random import choice

print("".join([choice(string.ascii_lowercase + string.digits) for i in range(8)]))

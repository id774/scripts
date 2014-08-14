#!/usr/bin/env python

import sys
import string
from random import choice

def generate_passwd(length):
    print("".join([choice(string.ascii_lowercase + string.digits)
                   for i in range(int(length))]))

if __name__ == '__main__':
    if len(sys.argv) != 2:
        generate_passwd(8)
    else:
        generate_passwd(sys.argv[1])

#!/usr/bin/env python

########################################################################
# simple_password.py: Simple Password Generator
#
#  Description:
#  This script generates a simple random password. It includes letters,
#  digits, and some special characters.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2013-01-07
#       Initial release.
#
#  Usage:
#  simple_password.py [length]
#  Default length is 8 if not specified.
#
########################################################################

import sys
import string
from random import choice

def generate_passwd(length=8):
    """Generate a random password of specified length."""
    chars = string.ascii_letters + string.digits + '_-!#&'
    print("".join([choice(chars) for i in range(int(length))]))

if __name__ == '__main__':
    length = sys.argv[1] if len(sys.argv) == 2 else 8
    generate_passwd(length)


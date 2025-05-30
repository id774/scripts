#!/usr/bin/env python

########################################################################
# simple_password.py: Simple Password Generator in Python
#
#  Description:
#  This script generates a random password. It can optionally exclude
#  special symbols from the password.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2023-12-06
#       Added option to exclude special symbols and improved argument handling.
#  v1.0 2013-01-07
#       Initial release.
#
#  Usage:
#  python simple_password.py [options] length
#  Options:
#    -s, --no-symbols: Do not include symbols in the password
#
########################################################################

import string
import sys
from optparse import OptionParser
from random import choice


def generate_passwd(length, use_symbols=True):
    """Generate a random password of specified length."""
    chars = string.ascii_letters + string.digits
    if use_symbols:
        chars += '_-!#&'
    print("".join([choice(chars) for i in range(length)]))


if __name__ == '__main__':
    parser = OptionParser(usage="usage: %prog [options] length")
    parser.add_option("-s", "--no-symbols", action="store_false", dest="use_symbols", default=True,
                      help="Do not include symbols in the password")
    (options, args) = parser.parse_args()

    if len(args) != 1 or not args[0].isdigit():
        parser.print_help()
        print("[ERROR] Length must be a number.", file=sys.stderr)
        sys.exit(1)

    length = int(args[0])
    generate_passwd(length, options.use_symbols)

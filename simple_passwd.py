#!/usr/bin/env python

########################################################################
# simple_password.py: Simple Password Generator in Python
#
#  Description:
#  This script generates a random password. It can optionally exclude
#  special symbols from the password. If symbols are enabled, at least
#  one symbol is guaranteed to be included in the password.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.5 2025-07-03
#       Ensure at least one symbol is included when use_symbols is enabled.
#  v1.4 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2023-12-06
#       Added option to exclude special symbols and improved argument handling.
#  v1.0 2013-01-07
#       Initial release.
#
#  Usage:
#  simple_passwd.py [options] length
#
#  Options:
#    -h, --help        show this help message and exit
#    -s, --no-symbols  Do not include symbols in the password
#
#  Requirements:
#  - Python Version: 3.1 or later
#
########################################################################

import os
import string
import sys
from optparse import OptionParser
from random import choice


def usage():
    """ Display the script header as usage information and exit. """
    script_path = os.path.abspath(__file__)
    in_header = False
    try:
        with open(script_path, 'r', encoding='utf-8') as f:
            for line in f:
                if line.strip().startswith('#' * 10):
                    if not in_header:
                        in_header = True
                        continue
                    else:
                        break
                if in_header and line.startswith('#'):
                    if line.startswith('# '):
                        print(line[2:], end='')
                    else:
                        print(line[1:], end='')
    except Exception as e:
        print("Error reading usage information: %s" % str(e), file=sys.stderr)
        sys.exit(1)
    sys.exit(0)

def generate_passwd(length, use_symbols=True):
    """Generate a random password of specified length."""
    if length <= 0:
        print("[ERROR] Length must be greater than zero.", file=sys.stderr)
        sys.exit(1)

    base_chars = string.ascii_letters + string.digits
    symbol_chars = '_-!#&'

    if use_symbols:
        if length == 1:
            print(choice(symbol_chars))
            return

        password = [choice(symbol_chars)]
        chars = base_chars + symbol_chars
        password += [choice(chars) for _ in range(length - 1)]

        from random import shuffle
        shuffle(password)
        print("".join(password))
    else:
        print("".join([choice(base_chars) for _ in range(length)]))

def main():
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

    return 0


if __name__ == '__main__':
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()
    sys.exit(main())

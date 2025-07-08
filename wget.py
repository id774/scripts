#!/usr/bin/env python

########################################################################
# wget.py: Simple Python-based File Downloader
#
#  Description:
#  This Python script downloads a file from a given URL and saves it
#  locally. It mimics basic functionality of the wget command.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.4 2025-07-07
#       Define download_file function to allow test import and avoid skipping tests.
#  v1.3 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.2 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.1 2023-12-08
#       Removed f-strings for compatibility with Python versions below 3.6.
#  v1.0 2023-12-06
#       Initial release.
#
#  Usage:
#  python wget.py <URL>
#  Example: python wget.py http://example.com/file.txt
#
########################################################################

import os
import sys

import requests


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

def download_file(url):
    """Download the file from the given URL and save it."""
    response = requests.get(url)
    filename = url.split('/')[-1]

    with open(filename, 'wb') as file:
        file.write(response.content)

    return filename

def main(url):
    download_file(url)
    return 0


if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()
    sys.exit(main(sys.argv[1]))

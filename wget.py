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
    sys.exit(0)

def download_file(url):
    response = requests.get(url)
    filename = url.split('/')[-1]

    with open(filename, 'wb') as file:
        file.write(response.content)


if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    download_file(sys.argv[1])

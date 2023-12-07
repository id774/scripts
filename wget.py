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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
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

import sys
import requests

def usage():
    print("Usage: {} <URL>".format(sys.argv[0]))
    sys.exit(1)

def download_file(url):
    response = requests.get(url)
    filename = url.split('/')[-1]

    with open(filename, 'wb') as file:
        file.write(response.content)


if __name__ == "__main__":
    if len(sys.argv) != 2:
        usage()

    download_file(sys.argv[1])

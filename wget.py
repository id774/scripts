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
#       Added Python version check to ensure the script runs on Python 3.6 or higher.
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
    print(f"Usage: {sys.argv[0]} <URL>")
    sys.exit(1)

def download_file(url):
    response = requests.get(url)
    filename = url.split('/')[-1]

    with open(filename, 'wb') as file:
        file.write(response.content)


if __name__ == "__main__":
    # Check if Python version is 3.6 or higher, exit if not
    if not (sys.version_info.major > 3 or (sys.version_info.major == 3 and sys.version_info.minor >= 6)):
        print("This script requires Python 3.6 or higher!")
        sys.exit(1)

    if len(sys.argv) != 2:
        usage()

    download_file(sys.argv[1])

#!/usr/bin/env python

########################################################################
# download_instagram_single.py: Download Single Image from Instagram
#
#  Description:
#  This script downloads a single image from a given Instagram URL.
#  It uses BeautifulSoup to parse the page and extract the image URL.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2023-12-08
#       Added Python version check to ensure the script runs on Python 3.6 or higher.
#  v1.1 2023-12-06
#       Refactored for clarity, added English comments, and updated documentation.
#  v1.0 2023-02-26
#       Initial release.
#
#  Usage:
#  python download_instagram_single.py <Instagram URL> <filename>
#  Example: python download_instagram_single.py https://www.instagram.com/p/xxxx image.jpg
#
########################################################################

import requests
from bs4 import BeautifulSoup
import argparse
import sys

def download_image(url, filename):
    # Fetch the Instagram page
    response = requests.get(url)
    soup = BeautifulSoup(response.text, 'html.parser')

    # Retrieve the image URL
    image_url = soup.find('meta', {'property': 'og:image'})['content']

    # Download the image
    response = requests.get(image_url)
    with open(filename, 'wb') as f:
        f.write(response.content)

    print(f'Downloaded {filename}.')


if __name__ == '__main__':
    # Check if Python version is 3.6 or higher, exit if not
    if not (sys.version_info.major > 3 or (sys.version_info.major == 3 and sys.version_info.minor >= 6)):
        print("This script requires Python 3.6 or higher!")
        sys.exit(1)

    parser = argparse.ArgumentParser()
    parser.add_argument('url', help='Instagram URL')
    parser.add_argument('filename', help='Name of the file to save')
    args = parser.parse_args()

    download_image(args.url, args.filename)

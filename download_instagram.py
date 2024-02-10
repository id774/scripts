#!/usr/bin/env python

########################################################################
# download_instagram.py: Batch Download Photos from Instagram
#
#  Description:
#  This script batch downloads photos from a specified Instagram account.
#  It uses the instaloader library to fetch all photo URLs from the account
#  and downloads them locally.
#  If no account is specified as an argument, it uses the current
#  directory name as the Instagram account name.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.0 2024-02-10
#       Renamed script to download_instagram.py for expanded functionality.
#       Comprehensive refactoring for improved testability and maintainability.
#  v1.3 2023-12-25
#       Modified to use current directory name as default Instagram username.
#  v1.2 2023-12-08
#       Removed f-strings for compatibility with Python versions below 3.6.
#  v1.1 2023-12-06
#       Refactored for clarity, added English comments, and updated documentation.
#  v1.0 2023-02-26
#       Initial release.
#
#  Usage:
#  python download_instagram.py [Instagram username]
#  Example: python download_instagram.py username
#           (If no username is given, the current directory name is used)
#
########################################################################

import argparse
import os
import re
import time
import urllib.request

import instaloader


class InstagramPhotoDownloader:
    def __init__(self, username):
        """Initialize the downloader with the specified username."""
        self.username = username
        self.loader = instaloader.Instaloader()
        self.profile = instaloader.Profile.from_username(self.loader.context, username)

    def download(self):
        """Main method to download all available Instagram photos."""
        urls = self._get_instagram_photo_urls()
        self._print_download_info(len(urls))

        max_number = self._get_max_number('.')
        remaining_urls = list(reversed(urls))[max_number:]

        for i, url in enumerate(remaining_urls, start=max_number + 1):
            self._print_remaining_time(len(remaining_urls), i - max_number)
            self._download_and_save_image(url, i)

    def _get_instagram_photo_urls(self):
        """Fetch all photo URLs from the Instagram profile."""
        urls = []
        for post in self.profile.get_posts():
            urls += [node.display_url for node in post.get_sidecar_nodes()]
            if post.typename == "GraphImage":
                urls.append(post.url)
        return urls

    def _print_download_info(self, post_count):
        """Print information about the download session."""
        print('This account {} has {} image posts.'.format(self.username, post_count))
        print('Estimate processing time is about {} minutes.'.format(int(post_count / 60) + 1))

    def _get_max_number(self, directory_path):
        """Find the highest numbered file in the directory to avoid overwriting."""
        max_number = 0
        for filename in os.listdir(directory_path):
            number = self._extract_number_from_filename(filename)
            max_number = max(max_number, number)
        return max_number

    def _extract_number_from_filename(self, filename):
        """Extract the number from the filename."""
        match = re.search(r'\d+$', os.path.splitext(filename)[0])
        return int(match.group()) if match else 0

    def _print_remaining_time(self, total, current):
        """Print the remaining time for the download process."""
        remaining = total - current
        print("{} seconds ({} minutes) remaining to complete processing.".format(remaining, int(remaining / 60) + 1))

    def _download_and_save_image(self, url, file_num):
        """Download a single photo from a URL."""
        filename = "{}_{}.jpg".format(self.username, str(file_num).zfill(5))
        print("Downloading {}...".format(filename))
        urllib.request.urlretrieve(url, filename)
        time.sleep(1)  # Prevent too many requests in a short time

def main():
    parser = argparse.ArgumentParser(description='Download all photos from an Instagram account.')
    parser.add_argument('username', nargs='?', help='Instagram username', default=os.path.basename(os.getcwd()))
    args = parser.parse_args()

    downloader = InstagramPhotoDownloader(args.username)
    downloader.download()


if __name__ == '__main__':
    main()

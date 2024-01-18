#!/usr/bin/env python

########################################################################
# download_instagram_batch.py: Batch Download Photos from Instagram
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
#  python download_instagram_batch.py [Instagram username]
#  Example: python download_instagram_batch.py username
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
        # Initialize the downloader with the given username
        self.username = username
        self.L = instaloader.Instaloader()
        self.profile = instaloader.Profile.from_username(
            self.L.context, self.username)

    def download(self):
        # Retrieve the URLs of all Instagram photos
        urls = self._get_instagram_photo_urls()
        post_count = len(urls)
        print('This account {} has {} image posts.'.format(
            self.username, post_count))
        minutes = int(post_count / 60) + 1
        print('Estimate processing time is about {} minutes.'.format(minutes))

        # Determine the starting index based on existing files
        max_number = self._get_max_number('.')

        for i, url in enumerate(reversed(urls)):
            if i >= max_number:
                time_left = "{} seconds".format(post_count - i)
                minutes_conv = "({} minutes)".format(
                    int((post_count - i) / 60) + 1)
                print("{} {} to complete processing.".format(
                    time_left, minutes_conv))
                file_num = str(i + 1).zfill(5)
                filename = "{}_{}.jpg".format(self.username, file_num)
                print("Downloading {}...".format(filename))
                self._download_image(url, filename)
                time.sleep(1)

    def _get_max_number(self, directory_path):
        # Determine the highest numbered file in the directory
        max_number = 0
        for filename in os.listdir(directory_path):
            basename = os.path.splitext(filename)[0]
            match = re.search(r'\d+$', basename)
            if match:
                number = int(match.group())
                if number > max_number:
                    max_number = number
        return max_number

    def _download_image(self, url, filename):
        # Download an image from the given URL
        urllib.request.urlretrieve(url, filename)

    def _get_instagram_photo_urls(self):
        # Fetch URLs of all photos from the Instagram profile
        urls = []
        for post in self.profile.get_posts():
            for image_url in post.get_sidecar_nodes():
                urls.append(image_url.display_url)
            if post.typename == "GraphImage":
                urls.append(post.url)
        return urls

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('username', nargs='?', help='Instagram username',
                        default=os.path.basename(os.getcwd()))
    args = parser.parse_args()

    downloader = InstagramPhotoDownloader(args.username)
    downloader.download()


if __name__ == '__main__':
    main()

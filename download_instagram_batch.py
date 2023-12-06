#!/usr/bin/env python

########################################################################
# download_instagram_batch.py: Batch Download Photos from Instagram
#
#  Description:
#  This script batch downloads photos from a specified Instagram account.
#  It uses the instaloader library to fetch all photo URLs from the account
#  and downloads them locally.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-06
#       Refactored for clarity, added English comments, and updated documentation.
#  v1.0 2023-02-26
#       Initial release.
#
#  Usage:
#  python download_instagram_batch.py <Instagram username>
#  Example: python download_instagram_batch.py username
#
########################################################################

import instaloader
import time
import urllib.request
import argparse
import os
import re

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
        print(f'This account has {post_count} image posts.')
        minutes = int(post_count / 60) + 1
        print(f'Estimate processing time is about {minutes} minutes.')

        # Determine the starting index based on existing files
        max_number = self._get_max_number('.')

        for i, url in enumerate(reversed(urls)):
            if i >= max_number:
                time_left = f"{post_count-i} seconds"
                minutes_conv = f"({int((post_count-i)/60)+1} minutes)"
                print(f"{time_left} {minutes_conv} to complete processing.")
                file_num = f"{i+1}".zfill(5)
                filename = f"{self.username}_{file_num}.jpg"
                print(f"Downloading {filename}...")
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
    parser.add_argument('username', help='Instagram username')
    args = parser.parse_args()

    downloader = InstagramPhotoDownloader(args.username)
    downloader.download()


if __name__ == '__main__':
    main()

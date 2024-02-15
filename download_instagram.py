#!/usr/bin/env python

########################################################################
# download_instagram.py: Batch Download Photos from Instagram
#
#  Description:
#  This script enables batch downloading of photos from a specified
#  Instagram account. It organizes downloads chronologically, ensuring
#  even pinned posts are sorted by their original post dates. The script
#  leverages the instaloader library to obtain photo URLs and associated
#  post IDs, facilitating incremental downloads. This approach prevents
#  re-downloading of already acquired photos by incorporating post IDs
#  into filenames, thus ensuring each download is unique.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.1 2024-02-15
#       Added chronological download feature with support for pinned posts.
#       Adjusted the download strategy to include post IDs in filenames,
#       supporting incremental updates.
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
#           (If no username is given, the script uses the current directory name)
#
########################################################################

import argparse
import os
import sys
import time
import urllib.request

try:
    import instaloader
    INSTALOADER_AVAILABLE = True
except ImportError:
    INSTALOADER_AVAILABLE = False

class InstagramPhotoDownloader:
    def __init__(self, username):
        if not INSTALOADER_AVAILABLE:
            print("Instaloader is not available. Functionality will be limited.")
            sys.exit(1)
        self.username = username
        self.loader = instaloader.Instaloader()
        self.profile = instaloader.Profile.from_username(self.loader.context, username)

    def download(self):
        # Fetch URLs and post IDs for all Instagram photos of the specified account
        urls_post_ids = self._get_instagram_photo_urls()
        total_images = len(urls_post_ids)
        print('This account {} has {} image posts to download.'.format(self.username, total_images))
        # Estimate and print the total processing time in minutes
        print('Estimated processing time is about {} minutes.'.format(int(total_images / 60) + 1))

        # Create a set of existing filenames to avoid re-downloading images
        existing_files = {filename for filename in os.listdir('.') if filename.endswith('.jpg')}

        for index, (url, post_id) in enumerate(urls_post_ids, start=1):
            # Construct the filename using the account username and post ID
            filename = "{}_{}.jpg".format(self.username, post_id)
            # Skip downloading if the file already exists
            if filename in existing_files:
                print("{} is already downloaded. Skipping...".format(filename))
                continue
            # Calculate the number of remaining images and the approximate time left
            remaining_images = total_images - index
            estimated_minutes_left = int(remaining_images / 60)
            # Print the download status with the remaining number of images and approximate time left
            print("Downloading {}... ({} of {} remaining, approx. {} minutes left)".format(filename, remaining_images, total_images, estimated_minutes_left))
            # Download the image and save it with the constructed filename
            self._download_and_save_image((url, post_id))

            # Check if there are any images left to download and print the status
            if index < total_images:
                print("{} images left... (approx. {} minutes left)".format(remaining_images, estimated_minutes_left))
            else:
                # Notify when all images have been downloaded
                print("All images have been downloaded.")

        # Print a message upon completing all downloads
        print("Download completed.")

    def _get_instagram_photo_urls(self):
        posts_data = []
        for post in self.profile.get_posts():
            if post.typename == "GraphImage":
                posts_data.append((post.url, post.date, post.shortcode))
            else:
                for node in post.get_sidecar_nodes():
                    posts_data.append((node.display_url, post.date, post.shortcode))

        sorted_posts_data = sorted(posts_data, key=lambda x: x[1])
        return [(data[0], data[2]) for data in sorted_posts_data]

    def _download_and_save_image(self, url_post_id_tuple):
        url, post_id = url_post_id_tuple
        filename = "{}_{}.jpg".format(self.username, post_id)
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

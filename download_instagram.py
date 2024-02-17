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
#  into filenames, thus ensuring each download is unique. Additionally,
#  it now allows setting custom file permissions for the downloaded photos.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.3 2024-02-18
#       Fixed a bug to ensure all images from multi-image posts are downloaded.
#  v2.2 2024-02-17
#       Added functionality to set custom file permissions for downloaded photos
#       using the --permissions command-line argument.
#  v2.1 2024-02-15
#       Added chronological download feature with support for pinned posts.
#       Adjusted the download strategy to include post IDs in filenames,
#       supporting incremental updates.
#  v2.0 2024-02-10
#       Renamed script to download_instagram.py for expanded functionality.
#       Comprehensive refactoring for improved testability and maintainability.
#  [Further version history truncated for brevity]
#  v1.0 2023-02-26
#       Initial release.
#
#  Usage:
#  python download_instagram.py [Instagram username] [--permissions PERM]
#  Example: python download_instagram.py username --permissions 640
#           (If no username is given, the script uses the current directory name.
#           Default permissions are 640 if not specified.)
#
########################################################################

import argparse
import math
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
    def __init__(self, username, permissions=0o640):  # Set default permissions to 0o640
        if not INSTALOADER_AVAILABLE:
            print("Instaloader is not available. Functionality will be limited.")
            sys.exit(1)
        self.username = username
        self.permissions = permissions  # Store permissions
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

        for index, (url, _, post_id, image_index) in enumerate(urls_post_ids, start=1):
            # Construct the filename using the account username, post ID, and image index
            filename = "{}_{}_{}.jpg".format(self.username, post_id, str(image_index).zfill(2))

            # Skip downloading if the file already exists
            if filename in existing_files:
                continue

            # Calculate the number of remaining images and the approximate time left
            remaining_images = total_images - index
            estimated_minutes_left = math.ceil(remaining_images / 60.0)

            # Print the download status with the remaining number of images and approximate time left
            print("Downloading {}... ({} of {} remaining, approx. {} minutes left)".format(filename, remaining_images, total_images, estimated_minutes_left))

            # Download the image and save it with the constructed filename
            self._download_and_save_image(url, filename)

        # Print a message upon completing all downloads
        print("Download completed.")

    def _get_instagram_photo_urls(self):
        posts_data = []
        for post in self.profile.get_posts():
            if post.typename == "GraphImage":
                posts_data.append((post.url, post.date, post.shortcode, 1))  # Single image post with default index 1
            elif post.typename == "GraphSidecar":
                for index, node in enumerate(post.get_sidecar_nodes(), start=1):
                    posts_data.append((node.display_url, post.date, post.shortcode, index))  # Multiple images with index

        sorted_posts_data = sorted(posts_data, key=lambda x: x[1])
        return sorted_posts_data

    def _download_and_save_image(self, url, filename):
        urllib.request.urlretrieve(url, filename)
        os.chmod(filename, self.permissions)  # Set permissions for the downloaded file
        time.sleep(1)  # Prevent too many requests in a short time

def main():
    parser = argparse.ArgumentParser(description='Download all photos from an Instagram account.')
    parser.add_argument('username', nargs='?', help='Instagram username', default=os.path.basename(os.getcwd()))
    parser.add_argument('--permissions', type=lambda x: int(x, 0), default=0o640, help='File permissions (octal)')
    args = parser.parse_args()

    downloader = InstagramPhotoDownloader(args.username, args.permissions)
    downloader.download()


if __name__ == '__main__':
    main()

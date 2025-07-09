#!/usr/bin/env python

########################################################################
# insta_downloader.py: Batch Download Photos from Instagram
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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.8 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v2.7 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v2.6 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v2.5 2024-11-03
#       Added --sleep command-line argument to allow customizable sleep time between downloads.
#       Set default sleep time to 10 seconds to reduce request rate.
#       Updated remaining time calculation to include sleep time.
#  v2.4 2024-05-29
#       Added error handling for HTTP 401 Unauthorized and other HTTP errors.
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
#       Renamed script to insta_downloader.py for expanded functionality.
#       Comprehensive refactoring for improved testability and maintainability.
#  [Further version history truncated for brevity]
#  v1.0 2023-02-26
#       Initial release.
#
#  Usage:
#  python insta_downloader.py [Instagram username] [--permissions PERM] [--sleep TIME]
#  Example: python insta_downloader.py username --permissions 640 --sleep 10
#           (If no username is given, the script uses the current directory name.
#           Default permissions are 640 if not specified, and default sleep time is 10 seconds.)
#
########################################################################

import argparse
import math
import os
import sys
import time
import urllib.request
from urllib.error import HTTPError

try:
    import instaloader
    INSTALOADER_AVAILABLE = True
except ImportError:
    INSTALOADER_AVAILABLE = False


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

class InstagramPhotoDownloader:
    def __init__(self, username, permissions=0o640, sleep_time=10):
        """ Initialize with username, permissions, and custom sleep time. """
        if not INSTALOADER_AVAILABLE:
            print("[ERROR] Instaloader is not available. Functionality will be limited.", file=sys.stderr)
            sys.exit(1)
        self.username = username
        self.permissions = permissions  # Store permissions
        self.sleep_time = sleep_time  # Store custom sleep time
        self.loader = instaloader.Instaloader()

        # Try to get profile information
        try:
            self.profile = instaloader.Profile.from_username(self.loader.context, username)
        except instaloader.exceptions.ConnectionException as e:
            print("[ERROR] Failed to get profile: {}".format(e), file=sys.stderr)
            sys.exit(1)

    def download(self):
        # Fetch URLs and post IDs for all Instagram photos of the specified account
        try:
            urls_post_ids = self._get_instagram_photo_urls()
        except instaloader.exceptions.ConnectionException as e:
            print("[ERROR] Failed to get posts: {}".format(e), file=sys.stderr)
            sys.exit(1)

        total_images = len(urls_post_ids)
        estimated_download_time = 1  # Approximate time in seconds for each download (without sleep)
        processing_time_per_image = max(1, estimated_download_time + self.sleep_time)  # Ensure at least 1 second per image

        print('[INFO] This account {} has {} image posts to download.'.format(self.username, total_images))
        # Estimate and print the total processing time in minutes with updated calculation
        total_estimated_time_minutes = math.ceil((total_images * processing_time_per_image) / 60)
        print('[INFO] Estimated processing time is about {} minutes.'.format(total_estimated_time_minutes))

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
            # Calculate the remaining time based on processing time per image
            estimated_minutes_left = math.ceil((remaining_images * processing_time_per_image) / 60)

            # Print the download status with the remaining number of images and approximate time left
            print("[INFO] Downloading {}... ({} of {} remaining, approx. {} minutes left)".format(
                filename, remaining_images, total_images, estimated_minutes_left))

            try:
                self._download_and_save_image(url, filename)
            except HTTPError as e:
                if e.code == 401:
                    print("[ERROR] HTTP 401 Unauthorized Error. Exiting.", file=sys.stderr)
                    sys.exit(1)
                else:
                    print("[ERROR] HTTP error occurred: {}".format(e), file=sys.stderr)
                    sys.exit(1)

        # Print a message upon completing all downloads
        print("[INFO] Download completed.")

    def _get_instagram_photo_urls(self):
        """ Fetches and returns URLs and metadata for all posts in the specified account, sorted chronologically. """
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
        """ Downloads an image from the specified URL and saves it with the specified filename and permissions. """
        urllib.request.urlretrieve(url, filename)
        os.chmod(filename, self.permissions)  # Set permissions for the downloaded file
        time.sleep(self.sleep_time)  # Use custom sleep time to prevent too many requests in a short time

def main():
    # Define command-line arguments
    parser = argparse.ArgumentParser(description='Download all photos from an Instagram account.')
    parser.add_argument('username', nargs='?', help='Instagram username', default=os.path.basename(os.getcwd()))
    parser.add_argument('--permissions', type=lambda x: int(x, 0), default=0o640, help='File permissions (octal)')
    parser.add_argument('--sleep', type=int, default=10, help='Time to sleep between downloads in seconds')  # New sleep option
    args = parser.parse_args()

    # Initialize the downloader with provided arguments
    downloader = InstagramPhotoDownloader(args.username, args.permissions, args.sleep)
    downloader.download()

    return 0


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    sys.exit(main())

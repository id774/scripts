#!/usr/bin/env python

########################################################################
# insta_video_downloader.py: Batch Download Videos from Instagram
#
#  Description:
#  This script enables batch downloading of videos from a specified
#  Instagram account. It organizes downloads chronologically, ensuring
#  even pinned posts are sorted by their original post dates. The script
#  leverages the instaloader library to obtain video URLs and associated
#  post IDs, facilitating incremental downloads. This approach prevents
#  re-downloading of already acquired videos by incorporating post IDs
#  into filenames, thus ensuring each download is unique.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      insta_video_downloader.py [Instagram username] [--permissions PERM] [--sleep TIME]
#
#  Example:
#      insta_video_downloader.py username --permissions 640 --sleep 10
#
#  Requirements:
#  - Python Version: 3.2 or later
#  - Dependencies: instaloader
#
#  Version History:
#  v1.4 2025-07-01
#       Standardized termination behavior for consistent script execution.
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-14
#       Unify error and info message formatting with stderr and prefix tags.
#  v1.1 2025-02-17
#       Replaced f-strings with format() method for compatibility with older Python versions.
#       Enhanced code comments for better readability and maintainability.
#  v1.0 2025-02-15
#       Initial release: Separate script for video downloading.
#
########################################################################

import argparse
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

class InstagramVideoDownloader:
    """
    This class handles downloading videos from an Instagram account.
    It utilizes the instaloader library to fetch video URLs and metadata.
    """

    def __init__(self, username, permissions=0o640, sleep_time=10):
        if not INSTALOADER_AVAILABLE:
            print("[ERROR] Instaloader is not available. Functionality will be limited.", file=sys.stderr)
            sys.exit(1)

        self.username = username
        self.permissions = permissions
        self.sleep_time = sleep_time
        self.loader = instaloader.Instaloader()

        try:
            self.profile = instaloader.Profile.from_username(self.loader.context, username)
        except instaloader.exceptions.ConnectionException as e:
            print("[ERROR] Failed to get profile: {}".format(e), file=sys.stderr)
            sys.exit(1)

    def download(self):
        """
        Download all video posts from the specified Instagram account.
        Videos are stored with filenames containing the post ID and index.
        """
        video_posts = self._get_instagram_video_urls()
        total_videos = len(video_posts)

        if total_videos == 0:
            print("[WARN] No videos found for {}. Exiting.".format(self.username))
            return

        print("[INFO] This account {} has {} video posts to download.".format(self.username, total_videos))
        existing_files = {filename for filename in os.listdir('.') if filename.endswith('.mp4')}

        for index, (url, _, post_id, video_index) in enumerate(video_posts, start=1):
            filename = "{}_{}_{}.mp4".format(self.username, post_id, str(video_index).zfill(2))

            if filename in existing_files:
                continue

            print("[INFO] Downloading {}... ({}/{})".format(filename, index, total_videos))

            try:
                self._download_and_save_video(url, filename)
            except HTTPError as e:
                print("[ERROR] HTTP error occurred: {}".format(e), file=sys.stderr)
                sys.exit(1)

        print("[INFO] Download completed.")

    def _get_instagram_video_urls(self):
        """
        Retrieve video URLs from the user's Instagram profile.
        Returns a sorted list of video metadata including URLs and post IDs.
        """
        video_data = []
        for post in self.profile.get_posts():
            if post.typename == "GraphVideo":
                video_data.append((post.video_url, post.date, post.shortcode, 1))
            elif post.typename == "GraphSidecar":
                for index, node in enumerate(post.get_sidecar_nodes(), start=1):
                    if node.is_video:
                        video_data.append((node.video_url, post.date, post.shortcode, index))

        sorted_video_data = sorted(video_data, key=lambda x: x[1])
        return sorted_video_data

    def _download_and_save_video(self, url, filename):
        """
        Download the video from the given URL and save it to disk.
        The file is also assigned the specified permissions.
        """
        urllib.request.urlretrieve(url, filename)
        os.chmod(filename, self.permissions)
        time.sleep(self.sleep_time)

def main():
    """
    Parse command-line arguments and initiate the video download process.
    """
    parser = argparse.ArgumentParser(description='Download all videos from an Instagram account.')
    parser.add_argument('username', nargs='?', help='Instagram username', default=os.path.basename(os.getcwd()))
    parser.add_argument('--permissions', type=lambda x: int(x, 0), default=0o640, help='File permissions (octal)')
    parser.add_argument('--sleep', type=int, default=10, help='Time to sleep between downloads in seconds')
    args = parser.parse_args()

    downloader = InstagramVideoDownloader(args.username, args.permissions, args.sleep)
    downloader.download()

    return 0


if __name__ == '__main__':
    if len(sys.argv) > 1 and sys.argv[1] in ('-h', '--help', '-v', '--version'):
        usage()

    if sys.version_info < (3, 2):
        print("[ERROR] This script requires Python 3.2 or later.", file=sys.stderr)
        sys.exit(9)

    sys.exit(main())

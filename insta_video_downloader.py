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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-02-15
#       Initial release: Separate script for video downloading.
#
#  Usage:
#  python insta_video_downloader.py [Instagram username] [--permissions PERM] [--sleep TIME]
#  Example: python insta_video_downloader.py username --permissions 640 --sleep 10
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

class InstagramVideoDownloader:
    def __init__(self, username, permissions=0o640, sleep_time=10):
        if not INSTALOADER_AVAILABLE:
            print("Instaloader is not available. Functionality will be limited.")
            sys.exit(1)

        self.username = username
        self.permissions = permissions
        self.sleep_time = sleep_time
        self.loader = instaloader.Instaloader()

        try:
            self.profile = instaloader.Profile.from_username(self.loader.context, username)
        except instaloader.exceptions.ConnectionException as e:
            print(f"Failed to get profile: {e}")
            sys.exit(1)

    def download(self):
        video_posts = self._get_instagram_video_urls()
        total_videos = len(video_posts)

        if total_videos == 0:
            print(f"No videos found for {self.username}. Exiting.")
            return

        print(f'This account {self.username} has {total_videos} video posts to download.')
        existing_files = {filename for filename in os.listdir('.') if filename.endswith('.mp4')}

        for index, (url, _, post_id, video_index) in enumerate(video_posts, start=1):
            filename = f"{self.username}_{post_id}_{str(video_index).zfill(2)}.mp4"

            if filename in existing_files:
                continue

            print(f"Downloading {filename}... ({index}/{total_videos})")

            try:
                self._download_and_save_video(url, filename)
            except HTTPError as e:
                print(f"HTTP error occurred: {e}")
                sys.exit(1)

        print("Download completed.")

    def _get_instagram_video_urls(self):
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
        urllib.request.urlretrieve(url, filename)
        os.chmod(filename, self.permissions)
        time.sleep(self.sleep_time)

def main():
    parser = argparse.ArgumentParser(description='Download all videos from an Instagram account.')
    parser.add_argument('username', nargs='?', help='Instagram username', default=os.path.basename(os.getcwd()))
    parser.add_argument('--permissions', type=lambda x: int(x, 0), default=0o640, help='File permissions (octal)')
    parser.add_argument('--sleep', type=int, default=10, help='Time to sleep between downloads in seconds')
    args = parser.parse_args()

    downloader = InstagramVideoDownloader(args.username, args.permissions, args.sleep)
    downloader.download()


if __name__ == '__main__':
    main()

#!/usr/bin/env python

import instaloader
import time
import urllib.request
import argparse
import os
import re

class InstagramPhotoDownloader:
    def __init__(self, username):
        self.username = username
        self.L = instaloader.Instaloader()
        self.profile = instaloader.Profile.from_username(self.L.context, self.username)

    def download(self):
        urls = self._get_instagram_photo_urls()
        post_count = len(urls)
        print(f'This account has {post_count} image posts.')
        minutes = int(post_count/60)+1
        print(f'Estimate processing time is {minutes} minutes.')

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
        urllib.request.urlretrieve(url, filename)

    def _get_instagram_photo_urls(self):
        urls = []
        for post in self.profile.get_posts():
            for image_url in post.get_sidecar_nodes():
                urls.append(image_url.display_url)
            if post.typename == "GraphImage":
                urls.append(post.url)
        return urls

def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('username', help='Instagram アカウントのユーザー名')
    args = parser.parse_args()

    downloader = InstagramPhotoDownloader(args.username)
    downloader.download()

if __name__ == '__main__':
    main()


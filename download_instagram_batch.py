#!/usr/bin/env python

import instaloader # pip install instaloader
import time
import urllib.request
import argparse

def download_image(url, filename):
    urllib.request.urlretrieve(url, filename)

def get_instagram_photo_urls(username):
    L = instaloader.Instaloader()
    profile = instaloader.Profile.from_username(L.context, username)

    urls = []
    for post in profile.get_posts():
        for image_url in post.get_sidecar_nodes():
            urls.append(image_url.display_url)
        if post.typename == "GraphImage":
            urls.append(post.url)

    return urls

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('username', help='Instagram アカウントのユーザー名')
    args = parser.parse_args()

    username = args.username
    urls = get_instagram_photo_urls(username)
    post_count = len(urls)
    print(f'This account has {post_count} posts')

    for i, url in enumerate(reversed(urls)):
        time_left = f"{post_count-i} seconds"
        print(f"{time_left} to complete processing.")
        filename = f"{username}_{i+1}.jpg"
        print(f"Downloading {filename}...")
        download_image(url, filename)
        time.sleep(1)


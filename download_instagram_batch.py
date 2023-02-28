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
    print(f'This account has {post_count} image posts.')
    minutes = int(post_count/60)+1
    print(f'Estimate processing time is {minutes} minutes.')

    for i, url in enumerate(reversed(urls)):
        time_left = f"{post_count-i} seconds"
        minutes_conv = f"({int((post_count-i)/60)+1} minutes)"
        print(f"{time_left} {minutes_conv} to complete processing.")
        file_num = f"{i+1}".zfill(8)
        filename = f"{username}_{file_num}.jpg"
        print(f"Downloading {filename}...")
        download_image(url, filename)
        time.sleep(1)


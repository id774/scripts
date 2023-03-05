#!/usr/bin/env python

import instaloader # pip install instaloader
import time
import urllib.request
import argparse
import os
import re

def get_max_number(directory_path):
    max_number = 0  # 最大の数字を格納する変数を初期化

    # ディレクトリ内のファイルを走査する
    for filename in os.listdir(directory_path):
        # ファイル名の末尾にある数字を正規表現で抽出する
        basename = os.path.splitext(filename)[0]
        match = re.search(r'\d+$', basename)
        if match:
            # 数値に変換し、最大値と比較する
            number = int(match.group())
            if number > max_number:
                max_number = number
    return max_number

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

    max_number = get_max_number('.')

    for i, url in enumerate(reversed(urls)):
        if i >= max_number:
            time_left = f"{post_count-i} seconds"
            minutes_conv = f"({int((post_count-i)/60)+1} minutes)"
            print(f"{time_left} {minutes_conv} to complete processing.")
            file_num = f"{i+1}".zfill(5)
            filename = f"{username}_{file_num}.jpg"
            print(f"Downloading {filename}...")
            download_image(url, filename)
            time.sleep(1)


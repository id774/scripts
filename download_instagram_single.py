#!/usr/bin/env python

import requests
from bs4 import BeautifulSoup
import argparse

def download_image(url, filename):
    # Instagramのページを取得
    response = requests.get(url)
    soup = BeautifulSoup(response.text, 'html.parser')

    # 画像のURLを取得
    image_url = soup.find('meta', {'property': 'og:image'})['content']

    # 画像をダウンロード
    response = requests.get(image_url)
    with open(filename, 'wb') as f:
        f.write(response.content)

    print(f'{filename} をダウンロードしました。')

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('url', help='Instagram の URL')
    parser.add_argument('filename', help='保存するファイル名')
    args = parser.parse_args()

    url = args.url
    filename = args.filename
    download_image(url, filename)


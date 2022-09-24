#!/bin/bash

# スクリーンショットの撮影時に影を含めない
defaults write com.apple.screencapture disable-shadow -boolean true

# Finder で隠しファイルを表示する
defaults write com.apple.finder AppleShowAllFiles true

# スクリーンショットのファイル名変更
defaults write com.apple.screencapture name "Screenshot"

# SystemUIServer を再起動して設定を反映させる
killall SystemUIServer


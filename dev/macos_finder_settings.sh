#!/bin/bash

# スクリーンショットの撮影時に影を含めない
defaults write com.apple.screencapture disable-shadow -boolean true

# Finder で隠しファイルを表示する
defaults write com.apple.finder AppleShowAllFiles true

# SystemUIServer を再起動して設定を反映させる
killall SystemUIServer


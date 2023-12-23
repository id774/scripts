#!/bin/sh

os=$(uname)

if [ "$os" = "Darwin" ]; then
  # スクリーンショットの撮影時に影を含めない
  defaults write com.apple.screencapture disable-shadow -boolean true

  # Finder で隠しファイルを表示する
  defaults write com.apple.finder AppleShowAllFiles true

  # スクリーンショットのファイル名変更
  defaults write com.apple.screencapture name "Screenshot"

  # 共有フォルダで .DS_Store ファイルを作成しない
  defaults write com.apple.desktopservices DSDontWriteNetworkStores true

  # SystemUIServer を再起動して設定を反映させる
  killall SystemUIServer
else
  exit 1
fi

exit 0

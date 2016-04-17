#!/bin/sh

brew doctor

brew update

brew install openssl
brew link openssl --force

brew install wget
brew install nkf
brew install vim
brew install freetype
brew install mecab
brew install cabocha
brew install ta-lib

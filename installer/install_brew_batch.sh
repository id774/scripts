#!/bin/sh

########################################################################
# install_brew_batch.sh: Bulk Homebrew Library Install Script
#
#  Description:
#  This script installs a set of essential tools and libraries on macOS
#  using Homebrew. It ensures that required software is installed and
#  configured for development, text processing, system administration,
#  and other tasks.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-01-16
#       Initial release. Added support for installing essential tools like
#       OpenSSL, Coreutils, Vim, and MeCab via Homebrew.
#
#  Usage:
#  Run this script in a terminal to set up your macOS environment.
#  Examples:
#     ./install_brew_batch.sh
#
#  Requirements:
#  - Homebrew must be installed prior to executing this script.
#
#  Exit Codes:
#  0: Success - All packages were installed successfully.
#  1: Error - Homebrew is not installed or a critical issue occurred.
#
#  Notes:
#  - This script ensures the use of GNU Coreutils on macOS for consistent
#    behavior across platforms.
#  - Force-links OpenSSL to ensure compatibility with applications requiring
#    the latest version.
#  - `trash` is installed for safer file deletions, replacing `rm`.
#
########################################################################

# Check if Homebrew is installed
if ! command -v brew >/dev/null 2>&1; then
  echo "Error: Homebrew is not installed. Please install Homebrew first." >&2
  exit 1
fi

# Check Homebrew environment
echo "Running 'brew doctor' to check the system's Homebrew environment..."
brew doctor

# Update Homebrew
echo "Updating Homebrew packages..."
brew update

# Install essential tools and libraries
echo "Installing essential tools and libraries using Homebrew..."
brew install openssl
brew link openssl --force
brew install wget
brew install nkf
brew install vim
brew install coreutils
brew install findutils
brew install trash
brew install freetype
brew install mecab
brew install cabocha
brew install ta-lib

echo "All specified packages have been successfully installed."

#!/bin/sh

########################################################################
# clear_chromium_cache.sh: Clear Chromium Browser Cache
#
#  Description:
#  This script provides options to remove Chromium's cache files.
#  It no longer forcefully terminates Chromium processes.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2024-08-11
#       Removed forceful termination of Chromium processes.
#       Added option to clear cache files. Display help message by default.
#  v1.1 2023-12-06
#       Refactored for clarity, added notes, and renamed the script.
#  v1.0 2016-01-08
#       Initial release.
#
#  Usage:
#  ./clear_chromium_cache.sh -c
#
#  Options:
#  -h   Display this help message.
#  -c   Clear Chromium cache files.
#
#  Notes:
#  - This script no longer forcefully terminates Chromium processes.
#  - Ensure that Chromium is closed before running with the -c option
#    to avoid potential data corruption.
#
########################################################################

# Function to display help message
display_help() {
  cat << EOF
Usage: $0 [-h] [-c]

Options:
  -h   Display this help message.
  -c   Clear Chromium cache files.

Notes:
  - This script no longer forcefully terminates Chromium processes.
  - Ensure that Chromium is closed before running with the -c option
    to avoid potential data corruption.
EOF
}

# Function to clear Chromium cache files
clear_cache() {
  cache_dir="$HOME/.config/chromium/Default"
  cache_files="Web Data Cache Code Cache GPUCache"

  for file in $cache_files; do
    file_path="$cache_dir/$file"
    if [ -e "$file_path" ]; then
      rm -rf "$file_path"
      echo "Cleared: $file"
    else
      echo "Not found: $file"
    fi
  done
}

# Parse options
while getopts "hc" opt; do
  case $opt in
    h)
      display_help
      exit 0
      ;;
    c)
      clear_cache
      exit 0
      ;;
    *)
      display_help
      exit 1
      ;;
  esac
done

# If no options are provided, display help
if [ $OPTIND -eq 1 ]; then
  display_help
  exit 0
fi

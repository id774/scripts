#!/bin/sh

########################################################################
# clear_chromium_cache.sh: Clear Chromium Browser Cache
#
#  Description:
#  This script provides options to remove Chromium's "Web Data" directory.
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
#       Added option to clear "Web Data" directory. Display help message by default.
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
#  -c   Clear Chromium "Web Data" directory.
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
  -c   Clear Chromium "Web Data" directory.

Notes:
  - This script no longer forcefully terminates Chromium processes.
  - Ensure that Chromium is closed before running with the -c option
    to avoid potential data corruption.
EOF
}

# Function to clear Chromium "Web Data" directory
clear_cache() {
  cache_dir="$HOME/.config/chromium/Default/Web Data"

  if [ -e "$cache_dir" ]; then
    rm -rf "$cache_dir"
    echo "Cleared: $cache_dir"
  else
    echo "Not found: $cache_dir"
  fi
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
      exit 0
      ;;
  esac
done

# If no options are provided, display help
if [ $OPTIND -eq 1 ]; then
  display_help
  exit 0
fi

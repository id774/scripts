#!/bin/sh

########################################################################
# clear_chromium_cache.sh: Clear Chromium Browser Cache
#
#  Description:
#  This script forcefully terminates Chromium and removes its cache.
#
#  Notes:
#  - This script will immediately terminate all Chromium processes.
#  - It removes the 'Web Data' file in the Chromium config, clearing the cache.
#  - Ensure that no important data is lost by closing Chromium before running.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-06
#       Refactored for clarity, added notes, and renamed the script.
#  v1.0 2016-01-08
#       Initial release.
#
#  Usage:
#  ./clear_chromium_cache.sh
#
########################################################################

# Terminate all Chromium processes
pkill -9 chromium

# Remove the Chromium cache if it exists
if [ -f ~/.config/chromium/Default/Web\ Data ]; then
  rm -f ~/.config/chromium/Default/Web\ Data
  echo "Chromium cache cleared."
else
  echo "Chromium cache not found."
fi


#!/bin/sh

########################################################################
# purge_apt_cache.sh: Clean Up Removed APT Packages
#
#  Description:
#  This script creates and executes a script to purge residual config
#  files of removed APT packages in Debian-based systems.
#
#  Notes:
#  - This script is intended for Debian-based systems only.
#  - It will permanently remove residual config files of APT packages.
#  - Ensure to review the script before running it.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-06
#       Added system check, refactored for clarity, and added notes.
#  v1.0 2019-08-29
#       Initial release.
#
#  Usage:
#  ./purge_apt_cache.sh
#
########################################################################

# Check if the system is Debian-based
if [ ! -f /etc/debian_version ]; then
  echo "This script only runs on Debian-based systems."
  exit 1
fi

# Generate and execute the cleanup script
SCRIPT_NAME="$TMP/purge_apt_cache.sh"

aptitude search . | grep '^c' | awk '{print $2}' | sed 's/^/sudo apt purge -y /g' > "$SCRIPT_NAME"
sed -i '1s/^/#!\/bin\/sh\n/' "$SCRIPT_NAME"
chmod +x "$SCRIPT_NAME"
"$SCRIPT_NAME"
rm "$SCRIPT_NAME"


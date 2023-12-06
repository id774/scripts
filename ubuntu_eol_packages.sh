#!/bin/sh

########################################################################
# ubuntu_eol_packages.sh: List Ubuntu Packages with End of Life Support
#
#  Description:
#  This script lists Ubuntu packages that have reached or are nearing
#  their end of life support. It requires dpkg and apt-cache commands.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-06
#       Added system environment check for Ubuntu.
#  v1.0 2012-05-08
#       Initial release.
#
#  Usage:
#  ./ubuntu_eol_packages.sh
#  Run this script on an Ubuntu system.
#
########################################################################

# Check if dpkg and apt-cache commands are available
command -v dpkg >/dev/null 2>&1 || { echo "dpkg command not found. Are you on Ubuntu?"; exit 1; }
command -v apt-cache >/dev/null 2>&1 || { echo "apt-cache command not found. Are you on Ubuntu?"; exit 1; }

# List Ubuntu packages with end of life support
dpkg -l | cut -d ' ' -f3 | tail -n +6 | xargs apt-cache show | egrep "^Pack|^Supp" | \
sed -e "s/^Package: /##/g" -e "s/Supported: /,/g" | tr -d '\n' | \
sed -e "s/##/\n/g" | sort | uniq


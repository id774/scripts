#!/bin/sh

########################################################################
# purge_kernels.sh: Purge Old Kernels in Ubuntu
#
#  Description:
#  This script removes old kernels from an Ubuntu or Ubuntu-based system,
#  keeping the currently running kernel. It ensures system compatibility
#  and prevents accidental removal of the active kernel.
#
#  The script performs the following operations:
#  - Checks if the system is Ubuntu-based before execution.
#  - Identifies and lists installed kernel packages.
#  - Excludes the currently running kernel from the removal list.
#  - Safely removes only outdated kernels while preserving system stability.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.2 2025-02-04
#       Switched from aptitude to apt for broader compatibility.
#       Improved kernel identification using dpkg --list.
#       Added a check to ensure old kernels exist before attempting removal.
#  v1.1 2023-12-06
#       Refactored for improved readability, naming, and system checking.
#  v1.0 2013-11-29
#       Initial release.
#
#  Usage:
#  Run the script directly without any arguments:
#      ./purge_kernels.sh
#
#  Notes:
#  - This script is intended for Ubuntu-based systems only.
#  - The currently running kernel will always be preserved.
#  - If no old kernels are found, the script exits without making changes.
#  - Ensure to review the listed kernels before executing the script.
#
########################################################################

# Ensure the system is Ubuntu-based
if [ ! -f /etc/lsb-release ] || [ ! -f /etc/debian_version ]; then
    echo "This script only runs on Ubuntu or Ubuntu-based systems." >&2
    exit 1
fi

# Get the current kernel version
CURKERNEL=$(uname -r | sed 's/-*[a-z]//g' | sed 's/-386//g')

# Find installed kernel packages
OLDKERNELS=$(dpkg --list | awk '/^ii/ && /linux-image-[0-9]/ {print $2}' | grep -v "$CURKERNEL")

# Ensure there are old kernels to remove
if [ -z "$OLDKERNELS" ]; then
    echo "No old kernels to remove."
    exit 0
fi

# Remove old kernels safely
echo "Purging old kernels: $OLDKERNELS"
sudo apt purge -y $OLDKERNELS

echo "Kernel cleanup completed."

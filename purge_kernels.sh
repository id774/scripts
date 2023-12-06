#!/bin/sh

########################################################################
# purge_kernels.sh: Purge Old Kernels in Ubuntu
#
#  Description:
#  This script removes old kernels from an Ubuntu or Ubuntu-based system,
#  keeping the currently running kernel. It checks for system compatibility
#  and only runs on Ubuntu or Ubuntu-based distributions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-06
#       Refactored for improved readability, naming, and system checking.
#  v1.0 2013-11-29
#       Initial release.
#
#  Usage:
#  sudo ./purge_kernels.sh
#
########################################################################

# Check if the system is Ubuntu or Ubuntu-based
if [ ! -f /etc/lsb-release ] || [ ! -f /etc/debian_version ]; then
  echo "This script only runs on Ubuntu or Ubuntu-based systems."
  exit 1
fi

# Get the current kernel version
CURKERNEL=$(uname -r | sed 's/-*[a-z]//g' | sed 's/-386//g')
LINUXPKG="linux-(image|headers|ubuntu-modules|restricted-modules)"
METALINUXPKG="linux-(image|headers|restricted-modules)-(generic|i386|server|common|rt|xen)"

# Find old kernels except the current one
OLDKERNELS=$(dpkg -l | awk '{print $2}' | grep -E $LINUXPKG | grep -vE $METALINUXPKG | grep -v $CURKERNEL)

# Purge old kernels
sudo aptitude purge $OLDKERNELS


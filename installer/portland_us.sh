#!/bin/sh

########################################################################
# portland_us.sh: User Directory Management Script for GNU/Linux
#
#  Description:
#  This script manages user directories on GNU/Linux systems. It installs
#  and configures xdg-user-dirs-gtk for managing user directories like
#  Documents, Downloads, Music, etc., and allows customization of these
#  directory paths.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-11-30
#       Updated to check if the environment is GNU/Linux and if 'apt' is available.
#       Changed package installation command from 'apt-get' to 'apt'.
#  v1.0 2010-07-27
#       Initial release. Basic functionality for managing user directories.
#
#  Usage:
#  Run the script without any arguments:
#      ./portland_us.sh
#
########################################################################

# Check if the system is GNU/Linux and 'apt' command exists
if [ "$(uname)" != "Linux" ] || ! command -v apt > /dev/null; then
    echo "This script requires a GNU/Linux system with the 'apt' command installed." >&2
    exit 1
fi

# Install xdg-user-dirs-gtk package
sudo apt install -y xdg-user-dirs-gtk

# Update user directories configuration
LANG=C xdg-user-dirs-gtk-update

# Open user directories configuration file for editing
vim $HOME/.config/user-dirs.dirs


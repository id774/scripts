#!/bin/sh

########################################################################
# munin_plugins_links.sh: Automate Munin Plugin Configuration
#
#  Description:
#  This script automates the configuration of Munin plugins by generating
#  and executing a script with commands provided by munin-node-configure.
#  It also handles specific plugin removal and service restart. It checks
#  for the presence of Munin before proceeding.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-06
#       Added check for Munin installation.
#  v1.0 2019-08-16
#       Initial release.
#
#  Usage:
#  ./munin_plugins_links.sh
#
########################################################################

# Check for Munin installation
if ! command -v munin-node-configure >/dev/null 2>&1; then
    echo "Munin is not installed. Please install Munin first."
    exit 1
fi

# Configuration variables
SCRIPT_NAME=$TMP/create-munin-plugins-links.sh
PLUGINS_DIR=/etc/munin/plugins

# Create a temporary script for Munin plugin setup
echo "#!/bin/sh" > $SCRIPT_NAME
sudo munin-node-configure --shell >> $SCRIPT_NAME
chmod +x $SCRIPT_NAME

# Execute the script in the plugins directory
cd $PLUGINS_DIR
sudo $SCRIPT_NAME
rm $SCRIPT_NAME

# Remove specific plugins
sudo rm ntp_[0-9]*

# Restart Munin node service
sudo systemctl restart munin-node.service


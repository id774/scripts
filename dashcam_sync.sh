#!/bin/sh

########################################################################
# dashcam_sync.sh: Dashcam Files Management Script
#
#  Description:
#  This script synchronizes dashcam files from a local directory to an
#  external drive and then moves them into a yearly organized folder.
#  It now loads source and destination directories from an external
#  configuration file located in either the 'etc' directory or its parent.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-23
#       Updated to load source and destination directories from an external
#       configuration file located in 'etc' or '../etc'.
#  v1.0 2023-12-05
#       Initial release. Adds directory checks, error handling, and
#       improves script reusability.
#
#  Usage:
#  Run the script without any arguments. Ensure that the dashcam_sync.conf
#  file is properly configured with the SOURCE_DIR and DEST_DIR variables,
#  located either in the 'etc' directory or its parent.
#      ./dashcam_sync.sh
#
########################################################################

# Load configuration from a .conf file
CONF_FILE="./etc/dashcam_sync.conf"
if [ ! -f "$CONF_FILE" ]; then
    CONF_FILE="$(dirname "${BASH_SOURCE[0]}")/../etc/dashcam_sync.conf"
    if [ ! -f "$CONF_FILE" ]; then
        echo "Configuration file not found."
        exit 5
    fi
fi
. "$CONF_FILE"

YEAR_DIR="$(date +"%Y")"

# Check if source and destination directories exist
if [ ! -d "$SOURCE_DIR" ] || [ ! -d "$DEST_DIR" ]; then
    echo "Error: Source or destination directory does not exist."
    exit 4
fi

# Check if source and destination directories exist
if [ ! -d "$SOURCE_DIR" ] || [ ! -d "$DEST_DIR" ]; then
    echo "Error: Source or destination directory does not exist."
    exit 3
fi

# Rsync files
echo "Synchronizing files to $DEST_DIR..."
rsync -avz --delete "$SOURCE_DIR/" "$DEST_DIR/daily/"
if [ $? -ne 0 ]; then
    echo "Error: Rsync failed."
    exit 2
fi

# Move files to yearly directory
move_files() {
    local from_dir=$1
    local to_dir=$2
    echo "Moving files from $from_dir to $to_dir..."
    mv "$from_dir"/* "$to_dir/"
    if [ $? -ne 0 ]; then
        echo "Error: Moving files failed."
        exit 1
    fi
}

move_files "$DEST_DIR/daily" "$DEST_DIR/$YEAR_DIR"
move_files "$SOURCE_DIR" "$SOURCE_DIR/../$YEAR_DIR"

echo "Operation completed successfully."


#!/bin/bash
#
########################################################################
# GPX File Management Script
#
#  Description:
#  This script manages GPX files by performing operations like copying to
#  specific directories, syncing with a remote server, and cleaning up
#  temporary files. It's tailored for cycling or running data management.
#
#  Author: id774
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 11/24,2023
#       Initial release. Features include checking for GPX files, copying them
#       to specific user and mounted directories, syncing with a remote server,
#       and removing files from the temporary directory.
#
# Usage:
#  Run the script without any arguments. Ensure that the TMP_DIR and USER_GPX_DIR
#  variables are set to the correct paths before running:
#      bash gpx_sync.sh
#
#  The script will automatically handle the copying, syncing, and removal of GPX files.
#
########################################################################

# Variables
TMP_DIR="$HOME/tmp"
USER_GPX_DIR="user3/gpx/strava"
RSYNC_USER="debian"
RSYNC_HOST="harpuia"
CURRENT_YEAR=$(date +"%Y")

# Function to check if GPX files exist in a directory
check_gpx_files() {
    local dir=$1
    if [ -z "$(ls -A $dir/*.gpx 2>/dev/null)" ]; then
        echo "No GPX files found in $dir. Exiting."
        return 1
    fi
}

# Function to copy files to a directory, creating the directory if it does not exist
copy_files() {
    local source=$1
    local destination=$2
    if [ ! -d "$destination" ]; then
        echo "Destination directory $destination does not exist. Exiting."
        return 1
    fi
    echo cp "$source" "$destination"
    cp "$source" "$destination"
}

# Function to perform rsync
sync_files() {
    local source=$1
    local destination_user=$2
    local destination_host=$3
    echo rsync -avz --delete "$source" "$destination_user@$destination_host:~/gpx/"
    rsync -avz --delete "$source" "$destination_user@$destination_host:~/gpx/"
}

# Function to remove files
remove_files() {
    local file_pattern=$1
    rm -v $file_pattern
}

# Main logic
# Check if there are GPX files in the TMP_DIR
check_gpx_files "$TMP_DIR" || exit 1

# Copy .gpx files to user3 directory
copy_files "$TMP_DIR"/*.gpx "$HOME/$USER_GPX_DIR/$CURRENT_YEAR/" || exit 1

# Copy .gpx files to mounted directory, skip if not mounted
copy_files "$TMP_DIR"/*.gpx "$HOME/mnt/sdb/$USER_GPX_DIR/$CURRENT_YEAR/" || exit 1

# Rsync files to the server
sync_files "$HOME/$USER_GPX_DIR" "$RSYNC_USER" "$RSYNC_HOST"

# Remove .gpx files from tmp directory
remove_files "$TMP_DIR"/*.gpx

echo "All operations completed successfully."

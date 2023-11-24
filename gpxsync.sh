#!/bin/bash

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
    cp "$source" "$destination"
}

# Function to perform rsync
sync_files() {
    local source=$1
    local destination_user=$2
    local destination_host=$3
    rsync -avz --delete "$source" "$destination_user@$destination_host:~/gpx/"
}

# Function to remove files
remove_files() {
    local file_pattern=$1
    rm $file_pattern
}

# Main logic
# Check if there are GPX files in the TMP_DIR
check_gpx_files "$TMP_DIR" || exit 1

# Copy .gpx files to user3 directory
copy_files "$TMP_DIR"/*.gpx "$HOME/$USER_GPX_DIR/$CURRENT_YEAR/" || exit 1

# Copy .gpx files to mounted directory, skip if not mounted
if mount | grep -q '/mnt/sdb'; then
    copy_files "$TMP_DIR"/*.gpx "$HOME/mnt/sdb/$USER_GPX_DIR/$CURRENT_YEAR/"
else
    echo "Mounted directory /mnt/sdb is not available. Skipping this step."
fi

# Rsync files to the server
sync_files "$HOME/$USER_GPX_DIR" "$RSYNC_USER" "$RSYNC_HOST"

# Remove .gpx files from tmp directory
remove_files "$TMP_DIR"/*.gpx

echo "All operations completed successfully."

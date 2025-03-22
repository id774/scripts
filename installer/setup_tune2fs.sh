#!/bin/sh

########################################################################
# setup_tune2fs.sh: Configure ext filesystem parameters using tune2fs
#
#  Description:
#  This script automates the configuration of ext-based filesystems
#  using tune2fs. It disables periodic checks, sets reserved blocks,
#  and applies settings for various storage configurations.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2025-03-13
#       Improved POSIX compliance and modularization.
#       Enhanced loop structures and variable handling.
#       Added progress output for better visibility.
#       Added system compatibility check for Linux.
#       Ensured tune2fs is installed before execution.
#       Redirected error messages to stderr for better logging and debugging.
#  [Further version history truncated for brevity]
#  v0.1 2011-09-26
#       First version.
#
#  Usage:
#  Run the script directly:
#      ./setup_tune2fs.sh
#  This script applies tune2fs settings to detected storage devices.
#
#  Notes:
#  - The script is designed for ext-based filesystems only.
#  - Ensure that tune2fs is installed before execution.
#  - Modifications apply to system partitions; review configurations beforehand.
#
#  Error Conditions:
#  - If the system is not Linux, the script exits with an error.
#  - If required commands are missing, the script exits with an error.
#  - If no applicable devices are found, execution halts.
#  - Errors from tune2fs should be resolved based on their output.
#
########################################################################

# Display script usage information
usage() {
    awk '
        BEGIN { in_usage = 0 }
        /^#  Usage:/ { in_usage = 1; print substr($0, 4); next }
        /^#{10}/ { if (in_usage) exit }
        in_usage && /^#/ { print substr($0, 4) }
    ' "$0"
    exit 0
}


# Check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "Error: This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Apply tune2fs settings if device is a block device
exec_tune2fs() {
    echo "Checking device: $1"
    if [ -b "$1" ]; then
        echo "Applying tune2fs settings to $1"
        sudo tune2fs -i 0 -c 0 -m 1 "$1"
    fi
}

# Apply tune2fs to standard /dev/sda partitions
set_sda() {
    echo "Processing /dev/sda partitions..."
    for i in $(seq 0 9); do
        exec_tune2fs "/dev/sda$i"
    done
}

# Apply tune2fs to LVM-based partitions
set_for_mapper() {
    for partition in root tmp var opt usr home data; do
        exec_tune2fs "$mapper-$partition"
        exec_tune2fs "$mapper--$partition"
    done
}

# Configure LVM partitions for Debian
set_lvm_debian() {
    mapper="/dev/mapper/$HOSTNAME_S"
    echo "Configuring LVM for Debian: $mapper"
    set_for_mapper
}

# Configure LVM partitions for RHEL
set_lvm_rhel() {
    mapper="/dev/mapper/vg_$HOSTNAME_S-lv_$HOSTNAME_S"
    echo "Configuring LVM for RHEL: $mapper"
    set_for_mapper
}

# Configure custom LVM layout
set_lvm_custom() {
    mapper="/dev/mapper/lv_$HOSTNAME_S"
    echo "Configuring custom LVM: $mapper"
    set_for_mapper
}

# Configure log-based volume names
set_lvm_logvol() {
    echo "Processing log-based LVM volumes..."
    for i in $(seq 0 9); do
        exec_tune2fs "/dev/mapper/vg_$HOSTNAME_S-LogVol0$i"
    done
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    echo "Starting tune2fs configuration..."
    check_system
    check_commands sudo tune2fs hostname
    check_sudo
    HOSTNAME_S=$(hostname -s)
    echo "Detected hostname: $HOSTNAME_S"
    set_sda
    set_lvm_debian
    set_lvm_rhel
    set_lvm_custom
    set_lvm_logvol
    echo "tune2fs configuration completed."
}

# Execute main function
main "$@"

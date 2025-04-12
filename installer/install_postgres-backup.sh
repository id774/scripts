#!/bin/sh

########################################################################
# install_postgres-backup.sh: Installer for PostgreSQL Backup Script
#
#  Description:
#  This script automates the setup of PostgreSQL backup by:
#  - Determining the correct PostgreSQL data directory.
#  - Deploying the cron job for automated backups.
#  - Ensuring the backup directory exists with proper permissions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-16
#       Unified structure, added system checks, improved error handling.
#  v0.2 2016-07-29
#       Change dirname, error check.
#  v0.1 2016-07-27
#       First.
#
#  Usage:
#  Run this script without arguments to install the PostgreSQL backup setup:
#      ./install_postgres-backup.sh
#
#  Requirements:
#  - The user must have `sudo`, `mkdir`, `cp`, and `chown` installed.
#  - This script is intended for Linux systems only.
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

# Function to check if the system is Linux
check_system() {
    if [ "$(uname -s)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
        exit 1
    fi
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the cron script." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Setup environment
setup_environment() {
    if [ -d /var/lib/postgresql ]; then
        TARGET=/var/lib/postgresql
    elif [ -d /var/lib/pgsql ]; then
        TARGET=/var/lib/pgsql
    else
        echo "[ERROR] PostgreSQL data directory not found." >&2
        exit 1
    fi
}

# Install backup script
install_script() {
    sudo cp -v "$SCRIPTS/cron/etc/cron.d/postgres-backup" /etc/cron.d/
    sudo mkdir -p "$TARGET/pg_dump"
    sudo chown postgres:postgres "$TARGET/pg_dump"
    sudo chmod 750 "$TARGET/pg_dump"
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_commands sudo mkdir cp chown chmod
    check_scripts
    setup_environment
    check_sudo
    install_script
    echo "PostgreSQL backup setup completed successfully."
}

# Execute main function
main "$@"

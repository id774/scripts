#!/bin/sh

########################################################################
# install_cassandra.sh: Installer for Apache Cassandra
#
#  Description:
#  This script automates the installation of Apache Cassandra by:
#  - Downloading the specified or default version from the official repository.
#  - Installing required dependencies.
#  - Configuring necessary directories and permissions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.4 2025-04-28
#       Add error handling to important operations to ensure safe execution flow.
#  v1.3 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-15
#       Unified structure, added system checks, improved error handling.
#  v0.1 2013-06-13
#       First.
#
#  Usage:
#  Run this script without arguments to install the default version (5.0.3):
#      ./install_cassandra.sh
#  Specify a version to install a different release:
#      ./install_cassandra.sh 4.1.8
#  Skip creating lib and log directories by adding a second argument:
#      ./install_cassandra.sh 4.1.8 -n
#
#  Requirements:
#  - Network connectivity is required to download the source files.
#  - The user must have `wget` and `sudo` installed.
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

# Function to check network connectivity
check_network() {
    if ! curl -s --head --connect-timeout 5 http://clients3.google.com/generate_204 >/dev/null; then
        echo "[ERROR] No network connection detected. Please check your internet access." >&2
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

# Setup version and environment
setup_environment() {
    VERSION="${1:-5.0.3}"
    OWNER="root:root"
}

# Create necessary directories for Cassandra
mkdir_lib_and_log() {
    echo "[INFO] Creating /var/lib/cassandra and /var/log/cassandra directories."

    if [ ! -d /var/lib/cassandra ]; then
        if ! sudo mkdir /var/lib/cassandra; then
            echo "[ERROR] Failed to create /var/lib/cassandra." >&2
            exit 1
        fi
    fi
    if ! sudo chown -R "$OWNER" /var/lib/cassandra; then
        echo "[ERROR] Failed to set ownership for /var/lib/cassandra." >&2
        exit 1
    fi

    if [ ! -d /var/log/cassandra ]; then
        if ! sudo mkdir /var/log/cassandra; then
            echo "[ERROR] Failed to create /var/log/cassandra." >&2
            exit 1
        fi
    fi
    if ! sudo chown -R "$OWNER" /var/log/cassandra; then
        echo "[ERROR] Failed to set ownership for /var/log/cassandra." >&2
        exit 1
    fi
}

# Install Apache Cassandra
install_cassandra() {
    setup_environment "$1"

    echo "[INFO] Creating temporary directory."
    mkdir install_cassandra
    cd install_cassandra || exit 1

    echo "[INFO] Downloading Apache Cassandra $VERSION."
    if ! wget "http://ftp.riken.jp/net/apache/cassandra/$VERSION/apache-cassandra-$VERSION-bin.tar.gz"; then
        echo "[ERROR] Failed to download Apache Cassandra version $VERSION." >&2
        exit 1
    fi

    echo "[INFO] Extracting archive."
    if ! tar xzvf "apache-cassandra-$VERSION-bin.tar.gz"; then
        echo "[ERROR] Failed to extract apache-cassandra-$VERSION-bin.tar.gz." >&2
        exit 1
    fi

    echo "[INFO] Renaming extracted directory."
    mv "apache-cassandra-$VERSION" "$VERSION"

    echo "[INFO] Preparing installation directory."
    [ -d /opt/cassandra ] || sudo mkdir -p /opt/cassandra

    if [ -d "/opt/cassandra/$VERSION" ]; then
        echo "[INFO] Removing directory: /opt/cassandra/$VERSION"
        sudo rm -rf "/opt/cassandra/$VERSION"
    fi

    echo "[INFO] Moving Cassandra to /opt."
    sudo mv "$VERSION" /opt/cassandra/
    sudo chown -R "$OWNER" "/opt/cassandra/$VERSION"

    echo "[INFO] Cleaning up temporary files."
    cd .. || exit 1
    rm -rf install_cassandra

    echo "[INFO] Creating 'current' symlink."
    cd /opt/cassandra || exit 1
    [ -L current ] || sudo ln -s "$VERSION" current
    sudo chown -h "$OWNER" current

    [ -n "$2" ] || mkdir_lib_and_log
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_system
    check_commands curl wget sudo tar mkdir mv rm
    check_network
    check_sudo
    install_cassandra "$@"

    echo "[INFO] Apache Cassandra $VERSION installed successfully."
}

# Execute main function
main "$@"

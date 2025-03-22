#!/bin/sh

########################################################################
# hadoop-start.sh: Hadoop Service Control Script
#
#  Description:
#  This script controls Hadoop services (NameNode, JobTracker, DataNode,
#  and TaskTracker) using the specified command (e.g., start, stop).
#  It ensures POSIX compatibility and includes error handling for missing arguments.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.6 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v0.5 2025-03-17
#       Encapsulated all logic into functions and introduced main function.
#  v0.4 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v0.3 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v0.2 2025-02-26
#       Improved POSIX compatibility by replacing `test -n` with `[ -n ]`.
#       Added error handling for missing arguments.
#  v0.1 2012-07-24
#       First release.
#
#  Usage:
#      ./hadoop-start.sh [start|stop] [Hadoop version]
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Function to define environment variables
set_environment() {
    HADOOP_VER=${2:-0.20}
    export JAVA_HOME=/opt/java/jdk
}

# Function to verify if Hadoop service scripts exist
verify_hadoop_scripts() {
    for service in namenode jobtracker datanode tasktracker; do
        if [ ! -x "/etc/init.d/hadoop-${HADOOP_VER}-$service" ]; then
            echo "Error: Hadoop service script not found or not executable: /etc/init.d/hadoop-${HADOOP_VER}-$service" >&2
            exit 1
        fi
    done
}

# Function to control Hadoop services
control_hadoop_services() {
    for service in namenode jobtracker datanode tasktracker; do
        sudo /etc/init.d/hadoop-${HADOOP_VER}-$service "$1"
    done
}

# Main function to execute the script
main() {
    if [ ! "$1" = "start" ] && [ ! "$1" = "stop" ]; then
        usage
    fi

    set_environment "$@"
    verify_hadoop_scripts
    check_sudo
    control_hadoop_services "$1"
}

# Execute main function
main "$@"

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
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v0.7 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
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

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
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
            echo "[ERROR] Hadoop service script not found or not executable: /etc/init.d/hadoop-${HADOOP_VER}-$service" >&2
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

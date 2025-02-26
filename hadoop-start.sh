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
#  v0.2 2025-02-26
#       Improved POSIX compatibility by replacing `test -n` with `[ -n ]`.
#       Added error handling for missing arguments.
#  v0.1 2012-07-24
#       First release.
#
#  Usage:
#  ./hadoop-start.sh [start|stop] [Hadoop version]
#
########################################################################

# Check if argument is provided
if [ -z "$1" ]; then
    echo "Error: No command provided. Usage: $0 [start|stop]" >&2
    exit 1
fi

# Define environment variables
HADOOP_VER=${2:-0.20}
export JAVA_HOME=/opt/java/jdk

# Control Hadoop services
sudo /etc/init.d/hadoop-${HADOOP_VER}-namenode "$1"
sudo /etc/init.d/hadoop-${HADOOP_VER}-jobtracker "$1"
sudo /etc/init.d/hadoop-${HADOOP_VER}-datanode "$1"
sudo /etc/init.d/hadoop-${HADOOP_VER}-tasktracker "$1"


#!/bin/sh

########################################################################
# get_resources.sh
# System Resources Information Gathering Script
#
#  Description:
#  This script gathers and displays various system resources and logs.
#  It adapts to the running environment (Linux or macOS) and uses
#  alternative commands or options when necessary.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-05
#       Refactored for macOS compatibility and command availability checks.
#  v1.0 2008-08-22
#       Initial release. Gathers system resources and log information.
#
#  Usage:
#  ./get_resources.sh
#
########################################################################

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to display a command's output if the command exists
execute_command() {
    if command_exists "$1"; then
        echo "[$1]"
        "$@"
        echo
    fi
}

# Function to display log file contents if the file exists
display_log() {
    if [ -f "$1" ]; then
        echo "[Contents of $1]"
        cat "$1"
        echo
    fi
}

# System Information
execute_command dmesg | grep "Linux version"
execute_command lsb_release -a

execute_command uname -a
execute_command uptime

# Conditional commands for macOS and Linux
if [ "$(uname)" = "Darwin" ]; then
    # macOS specific commands
    execute_command sysctl -n machdep.cpu.brand_string
    execute_command sysctl vm.swapusage
    execute_command vm_stat
    execute_command df -H
    execute_command top -l 1
    execute_command ps aux
else
    # Linux specific commands
    execute_command cat /proc/cpuinfo
    execute_command cat /proc/meminfo
    execute_command free -t
    execute_command vmstat -n
    execute_command df -P -T
    execute_command top -b -n 1
    execute_command ps -H auxZwww
    execute_command ps axl --sort -vsize | head -20
fi

# Network and process information
execute_command ip addr show
execute_command lsof -i
execute_command ss -t
execute_command netstat -t
execute_command netstat -s
execute_command netstat -an
execute_command w
execute_command lsmod
execute_command netstat -tan | grep ':80 ' | awk '{print $6}' | sort | uniq -c
execute_command ntpq -pn

# Log files
display_log "/var/log/auth.log"
display_log "/var/log/messages"
display_log "/var/log/fail2ban.log"


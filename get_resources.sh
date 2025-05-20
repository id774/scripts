#!/bin/sh

########################################################################
# get_resources.sh: System Resources Information Gathering Script
#
#  Description:
#  This script gathers and displays various system resources and logs.
#  It adapts to the running environment (Linux or macOS) and uses
#  alternative commands or options when necessary.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.7 2025-05-20
#       Suppress munin and git entries in auth.log using extended regex.
#  v1.6 2025-05-07
#       Enhance display_log function to support optional exclusion pattern as third argument.
#  v1.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.3 2025-03-17
#       Encapsulated all logic in functions and introduced main function.
#       Make POSIX compliant by removing 'local' variables.
#  v1.2 2024-12-04
#       Added error handling for command execution to display error messages when commands fail.
#       Added fail2ban status checks for non-Darwin environments.
#  v1.1 2023-12-05
#       Refactored for macOS compatibility and command availability checks.
#  v1.0 2008-08-22
#       Initial release. Gathers system resources and log information.
#
#  Usage:
#      ./get_resources.sh
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

# Function to check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Function to display a command's output if the command exists
execute_command() {
    if command_exists "$1"; then
        echo "[$@]"
        "$@" || echo "[ERROR] executing: $@"
        echo
    fi
}

# Function to display specific log file contents if the file exists
display_log() {
    if [ -f "$1" ]; then
        echo "[Contents of $1 with pattern '$2']"
        if [ -z "$3" ]; then
            grep "$2" "$1"
        else
            grep "$2" "$1" | grep -Ev "$3"
        fi
        echo
    fi
}

# Function to gather system information
gather_system_info() {
    if [ "$(uname)" != "Darwin" ]; then
        if [ "$(id -u)" -eq 0 ]; then
            dmesg | grep "Linux version" || true
        fi
    fi

    execute_command lsb_release -a
    execute_command uname -a
    execute_command uptime
}

# Function to gather OS-specific information
gather_os_specific_info() {
    if [ "$(uname)" = "Darwin" ]; then
        execute_command sysctl -n machdep.cpu.brand_string
        execute_command sysctl vm.swapusage
        execute_command vm_stat
        execute_command df -H
        execute_command top -l 1
        execute_command ps aux
    else
        execute_command cat /proc/cpuinfo
        execute_command cat /proc/meminfo
        execute_command free -t
        execute_command vmstat -n
        execute_command df -P -T
        execute_command top -b -n 1
        execute_command ps -H auxZwww
        execute_command ps axl --sort -vsize | head -20
    fi
}

# Function to gather network and process information
gather_network_info() {
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
}

# Function to check fail2ban status
check_fail2ban_status() {
    if [ "$(uname)" != "Darwin" ]; then
        execute_command fail2ban-client status
        execute_command fail2ban-client status sshd
    fi
}

# Function to gather logs
gather_logs() {
    display_log "/var/log/auth.log" "Accepted" "Accepted publickey for (munin|git)"
    display_log "/var/log/messages" "attack"
    display_log "/var/log/auth.log" '(Fail|refuse)'
    display_log "/var/log/fail2ban.log" "WARNING"
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    gather_system_info
    gather_os_specific_info
    gather_network_info
    check_fail2ban_status
    gather_logs
}

# Execute main function
main "$@"

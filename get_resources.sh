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
#  Usage:
#      ./get_resources.sh
#
#  Version History:
#  v2.1 2025-10-02
#       Stop monitoring fail2ban.log in this script as log handling is now managed separately.
#       Replace /proc/cpuinfo dump with concise CPU core count display.
#  v2.0 2025-08-07
#       Slim down redundant outputs and harden uname, grep, fail2ban, lsb_release, ip, and lsof handling.
#  v1.9 2025-08-04
#       Add pattern argument validation in display_log to prevent empty grep results.
#       Remove ad-hoc netstat grep section and consolidate all netstat calls under execute_command.
#  v1.8 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
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
########################################################################

OS_NAME=$(uname)

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if a command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# Display a command's output if the command exists
execute_command() {
    if command_exists "$1"; then
        echo "[$@]"
        "$@" || echo "[ERROR] executing: $@"
        echo
    fi
}

# Display specific log file contents if the file exists
display_log() {
    if [ -f "$1" ]; then
        if [ -z "$2" ]; then
            echo "[WARN] Skipping $1 because no pattern was provided." >&2
            return
        fi
        echo "[Contents of $1 with pattern '$2']"
        if [ -z "$3" ]; then
            grep -E "$2" "$1"
        else
            grep -E "$2" "$1" | grep -Ev "$3" || true
        fi
        echo
    fi
}

# Gather system information
gather_system_info() {
    if [ "$OS_NAME" != "Darwin" ]; then
        if [ "$(id -u)" -eq 0 ]; then
            dmesg | grep "Linux version" || true
        fi
        if command_exists lsb_release; then
            execute_command lsb_release -a
        fi
    fi
    execute_command uname -a
    execute_command uptime
    execute_command getconf _NPROCESSORS_ONLN
}

# Gather OS-specific information
gather_os_specific_info() {
    if [ "$OS_NAME" = "Darwin" ]; then
        execute_command sysctl -n machdep.cpu.brand_string
        execute_command sysctl vm.swapusage
        execute_command vm_stat
        execute_command df -H
        execute_command top -l 1
        execute_command ps aux
    else
        execute_command sh -c "grep -m1 'model name' /proc/cpuinfo | cut -d: -f2"
        execute_command cat /proc/meminfo
        execute_command vmstat
        execute_command df -P -T
        execute_command top -b -n 1
        execute_command ps aux
        execute_command ps axl --sort -vsize | head -20
    fi
}

# Gather network and process information
gather_network_info() {
    if command_exists ip; then
        execute_command ip addr show
    fi
    if command_exists lsof && [ "$(id -u)" -eq 0 ]; then
        execute_command lsof -i
    fi
    execute_command netstat -s
    execute_command netstat -an
    execute_command w
    execute_command ntpq -pn
}

# Check fail2ban status
check_fail2ban_status() {
    if [ "$OS_NAME" != "Darwin" ]; then
        if command_exists fail2ban-client && [ "$(id -u)" -eq 0 ]; then
            if fail2ban-client status | grep -q "sshd"; then
                execute_command fail2ban-client status
                execute_command fail2ban-client status sshd
            fi
        fi
    fi
}

# Gather logs
gather_logs() {
    display_log "/var/log/auth.log" "Accepted" "Accepted publickey for (munin|git)"
    display_log "/var/log/syslog" "attack"
    display_log "/var/log/auth.log" '(Fail|refuse)'
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    gather_system_info
    gather_os_specific_info
    gather_network_info
    check_fail2ban_status
    gather_logs
    return 0
}

# Execute main function
main "$@"
exit $?

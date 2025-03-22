#!/bin/sh

########################################################################
# fluent-start.sh: Fluentd Startup Script
#
#  Description:
#  This script starts Fluentd with a specified configuration. It sets up
#  Fluentd and executes it in the background, along with Fluent-Cat for
#  sending test messages. This version improves POSIX compatibility and
#  adds error handling for missing commands.
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
#  v0.4 2025-02-26
#       Improved POSIX compatibility by replacing `test -n` with `[ -n ]`.
#       Added error handling for missing Fluentd and Fluent-Cat commands.
#  v0.3 2023-12-20
#       Replaced 'which' with 'command -v' for command existence check.
#       Updated script header for consistency.
#  v0.2 2013-04-08
#       Add options.
#  v0.1 2013-04-04
#       First release.
#
#  Usage:
#      ./fluent-start.sh [fluentd path] [fluentd conf path] [options]
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

# Function to determine Fluentd and Fluent-Cat paths
determine_fluentd_paths() {
    if [ -n "$1" ]; then
        FLUENTD=$1/bin/fluentd
        FLUENT_CAT=$1/bin/fluent-cat
    else
        FLUENTD=$(command -v fluentd)
        FLUENT_CAT=$(command -v fluent-cat)
    fi
}

# Function to check if Fluentd and Fluent-Cat exist
check_fluentd_commands() {
    if [ -z "$FLUENTD" ]; then
        echo "Error: Fluentd not found. Please specify the path." >&2
        exit 1
    fi
    if [ -z "$FLUENT_CAT" ]; then
        echo "Error: Fluent-Cat not found. Please specify the path." >&2
        exit 1
    fi

    # Ensure Fluentd and Fluent-Cat are executable
    for cmd in "$FLUENTD" "$FLUENT_CAT"; do
        if [ ! -x "$cmd" ]; then
            echo "Error: Command not found or not executable: $cmd" >&2
            exit 1
        fi
    done
}

# Function to determine Fluentd configuration path
determine_fluentd_config() {
    if [ -n "$1" ]; then
        FLUENT_CONF=$1
    else
        FLUENT_CONF=~/.fluent
    fi
}

# Function to start Fluentd
start_fluentd() {
    $FLUENTD --setup "$FLUENT_CONF"
    $FLUENTD -c "$FLUENT_CONF/fluent.conf" "$3" &
}

# Function to send test message
send_test_message() {
    echo '{"json":"message"}' | "$FLUENT_CAT" debug.test
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    determine_fluentd_paths "$1"
    check_fluentd_commands
    determine_fluentd_config "$2"
    start_fluentd "$3"
    send_test_message
}

# Execute main function
main "$@"

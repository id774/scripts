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
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./fluent-start.sh [fluentd path] [fluentd conf path] [options]
#
#  Exit Status:
#  0: Fluentd setup and test-message send completed successfully.
#  1: Configuration, Fluentd setup, or test-message operation failed.
#  126: A required Fluentd command exists but is not executable.
#  127: A required Fluentd command is not installed.
#
#  Version History:
#  v1.1 2026-09-20
#       Preserve the CLI option argument, use shared prerequisite checks, and
#       propagate Fluentd setup and test-message failures.
#  v1.0 2026-07-11
#       Replace the awk {n,} interval expression in usage() with a portable
#       equivalent, since mawk on some systems matches it incorrectly.
#  v0.9 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v0.8 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v0.7 2025-03-25
#       Add config directory and file existence checks to determine_fluentd_config.
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
#       Initial release.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    check_commands awk
    awk '
        BEGIN { in_header = 0 }
        /^#+$/ && length($0) >= 10 { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if required commands are available and executable
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

# Determine Fluentd and Fluent-Cat paths
determine_fluentd_paths() {
    if [ -n "$1" ]; then
        FLUENTD=$1/bin/fluentd
        FLUENT_CAT=$1/bin/fluent-cat
    else
        FLUENTD=fluentd
        FLUENT_CAT=fluent-cat
    fi
}

# Check if Fluentd and Fluent-Cat exist
check_fluentd_commands() {
    check_commands "$FLUENTD" "$FLUENT_CAT"
}

# Determine Fluentd configuration path
determine_fluentd_config() {
    if [ -n "$1" ]; then
        FLUENT_CONF=$1
    else
        FLUENT_CONF="$HOME/.fluent"
    fi

    # Check if directory exists
    if [ ! -d "$FLUENT_CONF" ]; then
        echo "[ERROR] Configuration directory not found: $FLUENT_CONF" >&2
        exit 1
    fi

    # Check if fluent.conf exists
    if [ ! -f "$FLUENT_CONF/fluent.conf" ]; then
        echo "[ERROR] fluent.conf not found in: $FLUENT_CONF" >&2
        exit 1
    fi
}

# Start Fluentd
start_fluentd() {
    if ! "$FLUENTD" --setup "$FLUENT_CONF"; then
        echo "[ERROR] Fluentd setup failed." >&2
        return 1
    fi

    if [ -n "$1" ]; then
        "$FLUENTD" -c "$FLUENT_CONF/fluent.conf" "$1" &
    else
        "$FLUENTD" -c "$FLUENT_CONF/fluent.conf" &
    fi
    return 0
}

# Send test message
send_test_message() {
    if ! echo '{"json":"message"}' | "$FLUENT_CAT" debug.test; then
        echo "[ERROR] Failed to send Fluentd test message." >&2
        return 1
    fi
    return 0
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    determine_fluentd_paths "$1"
    check_fluentd_commands
    determine_fluentd_config "$2"

    if ! start_fluentd "$3"; then
        return 1
    fi

    if ! send_test_message; then
        return 1
    fi

    return 0
}

# Execute main function
main "$@"

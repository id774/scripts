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
#  ./fluent-start.sh [fluentd path] [fluentd conf path] [options]
#
########################################################################

# Determine Fluentd and Fluent-Cat paths
if [ -n "$1" ]; then
    FLUENTD=$1/bin/fluentd
    FLUENT_CAT=$1/bin/fluent-cat
else
    FLUENTD=$(command -v fluentd)
    FLUENT_CAT=$(command -v fluent-cat)
fi

# Check if Fluentd and Fluent-Cat exist
if [ -z "$FLUENTD" ]; then
    echo "Error: Fluentd not found. Please specify the path." >&2
    exit 1
fi
if [ -z "$FLUENT_CAT" ]; then
    echo "Error: Fluent-Cat not found. Please specify the path." >&2
    exit 1
fi

# Determine configuration path
if [ -n "$2" ]; then
    FLUENT_CONF=$2
else
    FLUENT_CONF=~/.fluent
fi

# Setup Fluentd configuration if needed
$FLUENTD --setup "$FLUENT_CONF"

# Start Fluentd
$FLUENTD -c "$FLUENT_CONF/fluent.conf" "$3" &

# Send test message
echo '{"json":"message"}' | "$FLUENT_CAT" debug.test

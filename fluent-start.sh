#!/bin/sh

########################################################################
# fluent-start.sh: Fluentd Startup Script
#
# Description:
# This script starts Fluentd with specified configuration. It sets up
# Fluentd and executes it in the background, along with Fluent-Cat for
# sending test messages.
#
# Author: id774 (More info: http://id774.net)
# Source Code: https://github.com/id774/scripts
# License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
# Contact: idnanashi@gmail.com
#
# Version History:
# v0.3 2023-12-20
#      Replaced 'which' with 'command -v' for command existence check.
#      Updated script header for consistency.
# v0.2 2013-04-08
#      Add options.
# v0.1 2013-04-04
#      First release.
#
# Usage:
# ./fluent-start.sh [fluentd path] [fluentd conf path] [options]
#
########################################################################

test -n "$1" && FLUENTD=$1/bin/fluentd
test -n "$1" || FLUENTD=$(command -v fluentd)
test -n "$1" && FLUENT_CAT=$1/bin/fluent-cat
test -n "$1" || FLUENT_CAT=$(command -v fluent-cat)
test -n "$2" && FLUENT_CONF=$2
test -n "$2" || FLUENT_CONF=~/.fluent

$FLUENTD --setup $FLUENT_CONF
$FLUENTD -c $FLUENT_CONF/fluent.conf $3 &
echo '{"json":"message"}' | $FLUENT_CAT debug.test

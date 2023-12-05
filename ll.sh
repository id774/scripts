#!/bin/sh

########################################################################
# Directory Size Summary Script
#
#  Description:
#  This script displays a detailed list of files and directories in the
#  current directory, including hidden files, and calculates the total
#  size of these files. It's useful for quickly estimating the size of
#  contents in a directory without including subdirectories. However,
#  it does not recursively account for the sizes within subdirectories
#  and may be seen as simplistic or limited for more complex directory
#  structures.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.0 2009-07-07
#       Initial release. Provides a summary of file sizes in the current
#       directory, including hidden files, but does not include sizes
#       within subdirectories.
#
########################################################################

ls -all | awk '{ print $0; x += $5 } END{ print "Total: " x }'

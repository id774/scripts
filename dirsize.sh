#!/bin/sh

########################################################################
# dirsize.sh: Directory Size Summary Script
#
#  Description:
#  This script displays a detailed list of files and directories in the
#  current directory, including hidden files, and calculates the total
#  size of these files in human-readable format (KB, MB, GB, TB, PB, EB,
#  ZB, YB, RB, QB). It's useful for quickly estimating the size of contents
#  in a directory without including subdirectories. The script does not
#  recursively account for sizes within subdirectories and is designed
#  for simplicity, providing a clear overview of the immediate directory
#  contents. Now updated to support file size display up to Quettabytes (QB).
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.1 2023-12-19
#       Updated to display file sizes in human-readable format up to QB,
#       including EB, ZB, YB, RB. Refactored code for improved readability
#       and maintainability.
#  v1.0 2009-07-07
#       Initial release. Provides a summary of file sizes in the current
#       directory, including hidden files, but does not include sizes
#       within subdirectories.
#
########################################################################

ls -all | awk '{
    print $0;
    x += $5
}
END {
    suffixes = "KB MB GB TB PB EB ZB YB RB QB";
    split(suffixes, units, " ");
    for (i = 1; x >= 1024 && i <= length(units); i++) {
        x /= 1024;
        suffix = units[i];
    }
    if (!suffix) suffix = "B";
    printf "Total: %.2f %s\n", x, suffix
}'


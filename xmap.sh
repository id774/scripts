#!/bin/sh

########################################################################
# xmap.sh: Apply ~/.Xmodmap using xmodmap if available
#
#  Description:
#  Load user's $HOME/.Xmodmap with xmodmap when both the command and the
#  file are available.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      xmap.sh
#
#  Options:
#    -h, --help       Show this help (prints the header block)
#    -v, --version    Show the version (prints the header block)
#
#  Version History:
#  v1.0 2025-09-05
#       Initial release.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
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

# Apply ~/.Xmodmap (assumes xmodmap exists)
apply_xmodmap() {
    file="$HOME/.Xmodmap"
    if [ ! -f "$file" ]; then
        echo "[INFO] ~/.Xmodmap not found; skipped."
        return 2
    fi

    echo "[INFO] Applying $file..."
    xmodmap -verbose "$file"
    return $?
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_commands xmodmap

    # Apply if file exists
    apply_xmodmap
    return $?
}

# Execute main function
main "$@"
exit $?

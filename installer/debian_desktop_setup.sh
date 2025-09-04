#!/bin/sh

########################################################################
# debian_desktop_setup.sh: Desktop Setup Launcher for Debian
#
#  Description:
#  Dispatch desktop environment setup to the corresponding script:
#    --xfce            -> debian_xfce_setup.sh
#    --gnome-flashback -> debian_gnome_flashback.sh
#
#  This script does not perform desktop package installation. It only
#  invokes per-DE setup scripts placed under $HOME/scripts/installer/.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./debian_desktop_setup.sh --xfce
#      ./debian_desktop_setup.sh --gnome-flashback
#
#  Notes:
#  - Run with exactly one option. When no option is given, this usage
#    text is displayed and the script exits.
#  - Expected script locations:
#      $HOME/scripts/installer/debian_xfce_setup.sh
#      $HOME/scripts/installer/debian_gnome_flashback_setup.sh
#
#  Error Conditions:
#  - Non-Linux system.
#  - Missing $HOME/scripts directory.
#  - Target setup script not found or not runnable.
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

# Verify script environment
setup_environment() {
    SCRIPTS="$HOME/scripts"
    if [ ! -d "$SCRIPTS" ]; then
        echo "[ERROR] Directory '$SCRIPTS' does not exist. Please create it or specify the correct path." >&2
        exit 1
    fi
}

# Resolve and run target script
run_target() {
    target="$1"
    if [ ! -f "$target" ]; then
        echo "[ERROR] Target script not found: $target" >&2
        exit 1
    fi
    if [ -x "$target" ]; then
        echo "[INFO] Running: $target"
        "$target"
        return $?
    fi
    # Fallback to sh if readable but not executable
    if [ -r "$target" ]; then
        echo "[WARN] Target not executable, invoking via sh: $target" >&2
        sh "$target"
        return $?
    fi
    echo "[ERROR] Target script is not readable: $target" >&2
    exit 1
}

# Parse CLI arguments; enforce exactly one mode
parse_args() {
    MODE=""
    while [ $# -gt 0 ]; do
        case "$1" in
            --xfce)
                if [ -n "$MODE" ]; then
                    echo "[ERROR] Specify exactly one option." >&2
                    usage
                fi
                MODE="xfce"
                ;;
            --gnome-flashback)
                if [ -n "$MODE" ]; then
                    echo "[ERROR] Specify exactly one option." >&2
                    usage
                fi
                MODE="gnome"
                ;;
            -h|--help|-v|--version)
                usage
                ;;
            *)
                usage
                ;;
        esac
        shift
    done

    if [ -z "$MODE" ]; then
        usage
    fi

    echo "$MODE"
}

# Main entry point of the script
main() {
    setup_environment

    MODE="$(parse_args "$@")"

    case "$MODE" in
        xfce)
            XFCE_SCRIPT="$HOME/scripts/installer/debian_xfce_setup.sh"
            run_target "$XFCE_SCRIPT"
            ;;
        gnome)
            GNOME_SCRIPT="$HOME/scripts/installer/debian_gnome_flashback_setup.sh"
            run_target "$GNOME_SCRIPT"
            ;;
        *)
            usage
            ;;
    esac
}

# Execute main function
main "$@"
exit $?

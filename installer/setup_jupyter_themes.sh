#!/bin/sh
#
########################################################################
# setup_jupyter_theme.sh: Setup Jupyter Theme on macOS/Linux
#
#  Description:
#  This script sets up Jupyter themes by:
#  - Installing jupyterthemes via pip.
#  - Configuring monokai theme settings.
#  - Ensuring environment readiness before execution.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.3 2025-04-26
#       Add error checks to Jupyter theme setup process.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-16
#       Added sudo validation, environment checks, and improved error handling.
#       Replaced direct vim execution with an instructional message.
#  v0.1 2021-08-06
#       First version.
#
#  Usage:
#  Run this script to install and apply Jupyter themes:
#      ./setup_jupyter_theme.sh [python_path] [-n]
#
#  Requirements:
#  - Must be executed on macOS/Linux.
#  - Requires `pip`, `jupyter`, and `jt` installed.
#  - Requires sudo privileges unless a second argument is provided.
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

# Function to check if required commands exist
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

# Function to check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges." >&2
        exit 1
    fi
}

# Setup environment
setup_environment() {
    PYTHON_PATH="${1:-/opt/python/current}"
    PIP="$PYTHON_PATH/bin/pip"
    JUPYTER="$PYTHON_PATH/bin/jupyter"
    JT="$PYTHON_PATH/bin/jt"
    if [ -z "$2" ] || [ "$2" = "sudo" ]; then
        SUDO="sudo -H"
    else
        SUDO=""
    fi
    [ "$SUDO" = "sudo -H" ] && check_sudo
}

# Install Jupyter theme
install_jupyter_theme() {
    if ! "$PIP" show jupyterthemes >/dev/null 2>&1; then
        echo "[ERROR] jupyterthemes is not installed. Install it manually before running this script." >&2
        exit 1
    fi

    echo "[INFO] Applying Jupyter theme settings..."
    if ! "$JT" -t monokai -f inconsolata -N -T -fs 10 -nfs 10 -ofs 10 -cellw 90% -lineh 140; then
        echo "[ERROR] Failed to apply Jupyter theme settings." >&2
        exit 1
    fi

    if ! echo "div.output_area img, div.output_area svg { background: #C6D3DF; }" >> "$HOME/.jupyter/custom/custom.css"; then
        echo "[ERROR] Failed to append custom CSS to Jupyter configuration." >&2
        exit 1
    fi

    echo "[INFO] Jupyter theme configuration applied successfully."
    echo "[INFO] If further customization is needed, edit: $HOME/.jupyter/custom/custom.css"
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    setup_environment "$@"
    check_commands "$PIP" "$JUPYTER" "$JT"
    install_jupyter_theme
    echo "[INFO] Jupyter theme setup completed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

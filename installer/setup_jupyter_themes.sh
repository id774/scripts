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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
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

# Function to check if required commands exist
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install it and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Function to check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges." >&2
        exit 1
    fi
}

# Setup environment
setup_environment() {
    PYTHON_PATH="${1:-/opt/python/current}"
    PIP="$PYTHON_PATH/bin/pip"
    JUPYTER="$PYTHON_PATH/bin/jupyter"
    JT="$PYTHON_PATH/bin/jt"
    SUDO="sudo -H"
    [ -n "$2" ] && SUDO=""
    [ "$SUDO" = "sudo -H" ] && check_sudo
}

# Install Jupyter theme
install_jupyter_theme() {
    if ! "$PIP" show jupyterthemes >/dev/null 2>&1; then
        echo "Error: jupyterthemes is not installed. Install it manually before running this script." >&2
        exit 1
    fi

    "$JT" -t monokai -f inconsolata -N -T -fs 10 -nfs 10 -ofs 10 -cellw 90% -lineh 140
    echo "div.output_area img, div.output_area svg { background: #C6D3DF; }" >> "$HOME/.jupyter/custom/custom.css"
    echo "Jupyter theme configuration applied. If further customization is needed, edit: $HOME/.jupyter/custom/custom.css"
}

# Main execution function
main() {
    setup_environment "$@"
    check_commands "$PIP" "$JUPYTER" "$JT"
    install_jupyter_theme
    echo "Jupyter theme setup completed successfully."
}

main "$@"

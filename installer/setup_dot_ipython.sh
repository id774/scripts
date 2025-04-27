#!/bin/sh

########################################################################
# setup_dot_ipython.sh: IPython Setup Script
#
#  Description:
#  This script automates the setup and configuration of the IPython environment.
#  It creates IPython profiles, copies necessary startup files, and sets appropriate
#  permissions.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v1.6 2025-04-27
#       Add strict error checking after all filesystem operations.
#  v1.5 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.4 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.3 2025-03-20
#       Improved POSIX compliance and safety checks.
#  v1.2 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#  v1.1 2023-12-20
#       Refactored script for readability and added documentation.
#  v1.0 2014-08-16
#       Initial release for automating IPython setup.
#
#  Usage:
#      ./setup_dot_ipython.sh
#
#  Run this script to initialize and configure your IPython environment.
#  This script sets up a default IPython profile and an additional 'nbserver' profile.
#  It also copies necessary startup files from a predefined 'SCRIPTS' directory.
#  Before running this script, ensure that the 'SCRIPTS' environment variable points
#  to your directory containing the IPython startup files.
#
#  Notes:
#  - This script is intended to be used with Zsh.
#  - Make sure to back up any existing IPython configuration before running this script.
#  - Ensure that the IPython is installed on your system.
#  - 'SCRIPTS' environment variable must be correctly set.
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

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the path of your IPython startup files." >&2
        exit 1
    fi
}

# Function to check required commands
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

# Check if IPython is installed and get its path
check_ipython() {
    if ! command -v ipython >/dev/null 2>&1; then
        echo "[ERROR] IPython is not installed." >&2
        exit 1
    fi
    echo "[INFO] IPython found at: $(command -v ipython)"
}

# Setup environment variables based on system type
setup_environment() {
    case "$(uname -s)" in
        Darwin)
            OPTIONS="-Rv"
            ;;
        *)
            OPTIONS="-Rvd"
            ;;
    esac
}

# Initialize the nbserver IPython profile
init_nbserver() {
    if ! ipython profile create nbserver; then
        echo "[ERROR] Failed to create IPython nbserver profile." >&2
        exit 1
    fi

    NB_SERVER_DIR="${HOME}/.ipython/profile_nbserver/startup"

    if [ -d "$NB_SERVER_DIR" ]; then
        if ! cp $OPTIONS "${SCRIPTS}/dot_files/dot_ipython/profile_default/startup/00-init.py" "$NB_SERVER_DIR"; then
            echo "[ERROR] Failed to copy 00-init.py to $NB_SERVER_DIR" >&2
            exit 1
        fi
        if [ -f "${NB_SERVER_DIR}/00-init.py" ] && [ -x "${NB_SERVER_DIR}/00-init.py" ]; then
            if ! chmod -x "${NB_SERVER_DIR}/00-init.py"; then
                echo "[ERROR] Failed to remove executable bit from 00-init.py in nbserver profile" >&2
                exit 1
            fi
        fi
    fi
}

# Copy .ipython configuration and set up default profile
copy_dotipython() {
    IPYTHON_DIR="${HOME}/.ipython"
    if [ -d "$IPYTHON_DIR" ]; then
        echo "[INFO] Removing existing IPython directory: $IPYTHON_DIR"
        if ! rm -rf "$IPYTHON_DIR"; then
            echo "[ERROR] Failed to remove existing IPython directory: $IPYTHON_DIR" >&2
            exit 1
        fi
    fi

    if ! ipython profile create default; then
        echo "[ERROR] Failed to create IPython default profile." >&2
        exit 1
    fi

    DEFAULT_PROFILE_DIR="${IPYTHON_DIR}/profile_default/startup"

    if [ ! -d "$DEFAULT_PROFILE_DIR" ]; then
        if ! mkdir -p "$DEFAULT_PROFILE_DIR"; then
            echo "[ERROR] Failed to create startup directory: $DEFAULT_PROFILE_DIR" >&2
            exit 1
        fi
    fi

    if ! cp $OPTIONS "${SCRIPTS}/dot_files/dot_ipython/profile_default/startup/00-init.py" "$DEFAULT_PROFILE_DIR"; then
        echo "[ERROR] Failed to copy 00-init.py to $DEFAULT_PROFILE_DIR" >&2
        exit 1
    fi

    if [ -f "${DEFAULT_PROFILE_DIR}/00-init.py" ] && [ -x "${DEFAULT_PROFILE_DIR}/00-init.py" ]; then
        if ! chmod -x "${DEFAULT_PROFILE_DIR}/00-init.py"; then
            echo "[ERROR] Failed to remove executable bit from 00-init.py" >&2
            exit 1
        fi
    fi

    if ! cp $OPTIONS "${SCRIPTS}/dot_files/dot_zshrc" "${HOME}/.zshrc"; then
        echo "[ERROR] Failed to copy dot_zshrc to ${HOME}/.zshrc" >&2
        exit 1
    fi

    if ! cp $OPTIONS "${SCRIPTS}/dot_files/dot_zshrc_local" "${HOME}/.zshrc_local"; then
        echo "[ERROR] Failed to copy dot_zshrc_local to ${HOME}/.zshrc_local" >&2
        exit 1
    fi
}

# Main function to execute the script
main() {
    case "$1" in
        -h|--help) usage ;;
    esac

    check_scripts
    check_ipython
    check_commands uname cp chmod rm mkdir
    setup_environment
    copy_dotipython "$@"
    init_nbserver "$@"

    echo "[INFO] dot_ipython setup successfully completed."
}

# Execute main function
main "$@"

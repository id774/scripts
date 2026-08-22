#!/bin/sh

########################################################################
# install_conda.sh: Bulk Python Library Install Script
#
#  Description:
#  This script sets up a Python development environment by installing
#  a compact set of libraries using Conda. It ensures that the common
#  dependencies for scientific computing, data analysis, machine
#  learning, and Hugging Face work are installed and ready for use.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script in a terminal to set up your Python environment.
#
#  Examples:
#     ./install_conda.sh /path/to/python
#     ./install_conda.sh
#
#  Requirements:
#  - Conda must be installed prior to executing this script.
#
#  Exit Status:
#  0: Success - All libraries were installed successfully.
#  1: Error - A general installation or update failure occurred.
#  126: Error - A required command exists but is not executable.
#  127: Error - A required command was not found.
#
#  Notes:
#  - If no path is provided, the script assumes the default installation
#    path for Anaconda (`$HOME/local/anaconda3`).
#  - Includes a focused set of tools for scientific computing, data
#    analysis, machine learning, and Hugging Face work.
#
#  Version History:
#  v2.0 2026-08-22
#       Slim the package set, remove Easy Install, and improve
#       portability and Conda failure handling.
#  v1.4 2026-07-11
#       Replace the awk {n,} interval expression in usage() with a portable
#       equivalent, since mawk on some systems matches it incorrectly.
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-01-16
#       Official release. Improved documentation, added command existence
#       checks, refined error handling.
#  v0.3 2018-05-10
#       Removed conda-forge channel.
#  v0.2 2018-05-02
#       Added conda-forge channel.
#  v0.1 2018-02-28
#       Initial version.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
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

# Set up the environment variables for Conda
setup_environment() {
    if [ -n "$1" ]; then
        export CONDA=$1/bin/conda
    else
        export CONDA=$HOME/local/anaconda3/bin/conda
    fi

    # Verify that Conda is available
    check_commands "$CONDA"
}

# Install a single Python library using Conda
install_lib() {
    echo "[INFO] Installing $1..."
    if ! $CONDA install -y "$1"; then
        echo "[ERROR] Failed to install $1." >&2
        return 1
    fi
}

# Install the necessary libraries using Conda
install_libs() {
    echo "[INFO] Updating Conda base environment..."
    if ! $CONDA update -n base -y conda; then
        echo "[ERROR] Failed to update Conda." >&2
        return 1
    fi

    echo "[INFO] Installing essential libraries using Conda..."
    # Define the list of libraries as a multi-line string
    libs="
    numpy
    scipy
    pandas
    scikit-learn
    matplotlib
    ipython
    jupyterlab
    pytorch
    transformers
    datasets
    huggingface_hub
    xgboost
    "

    # Loop through each library and install it
    for lib in $libs; do
        install_lib "$lib" || return 1
    done
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    echo "[INFO] Starting Conda environment setup and library installation..."
    setup_environment "$1"
    if ! install_libs; then
        return 1
    fi

    echo "[INFO] All specified conda packages have been installed."
    return 0
}

# Execute main function
main "$@"

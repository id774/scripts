#!/bin/sh

########################################################################
# install_dotvim.sh: Installer for dot_vim Configuration
#
#  Description:
#  This script automates the setup of the `.vim` directory by:
#  - Ensuring the required target directory exists.
#  - Copying configuration files from the `dot_vim` repository.
#  - Adjusting options based on the operating system type.
#  - Allowing a custom installation path via command-line argument.
#  - Additionally, if ~/.config/nvim exists, it will also receive the
#    same files (for NeoVim compatibility).
#  - Use --uninstall to remove installed dot_vim configurations.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script directly without any arguments for default installation:
#      ./install_dotvim.sh
#  Specify a custom installation path:
#      ./install_dotvim.sh /custom/path/to/.vim
#  Uninstall:
#      ./install_dotvim.sh --uninstall
#
#  Requirements:
#  - The `SCRIPTS` environment variable must be set to the directory
#    containing the `dot_vim` configuration files.
#  - Must be executed in a shell environment where `cp` and `vim` are available.
#
#  Version History:
#  v2.0 2025-08-01
#       Add --uninstall option to remove dot_vim and optionally nvim configuration.
#  v1.8 2025-07-15
#       Also copy files to ~/.config/nvim if it exists.
#  v1.7 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.6 2025-06-10
#       Always recreate .vim directory from scratch before installation to ensure clean setup.
#  v1.5 2025-04-26
#       Add critical failure checks to dot_vim installation steps.
#  v1.4 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v1.3 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.2 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.1 2025-03-14
#       Added command existence check and environment variable validation.
#  v1.0 2010-03-07
#       First version.
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

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the directory containing the dot_vim configuration files." >&2
        exit 1
    fi
}

# Set up environment variables
setup_environment() {
    TARGET="${1:-$HOME/.vim}"
    case "$(uname -s)" in
        Darwin)
            OPTIONS="-Rv"
            ;;
        *)
            OPTIONS="-Rvd"
            ;;
    esac
}

# Install dot_vim configuration
install_dotvim() {
    if [ -d "$TARGET" ]; then
        echo "[INFO] Removing existing vim directory: $TARGET"
        if ! rm -rf "$TARGET"; then
            echo "[ERROR] Failed to remove existing vim directory: $TARGET." >&2
            exit 1
        fi
    fi

    echo "[INFO] Creating target directory at $TARGET."
    if ! mkdir -p "$TARGET"; then
        echo "[ERROR] Failed to create target directory: $TARGET." >&2
        exit 1
    fi

    echo "[INFO] Copying dot_vim files to $TARGET."
    if ! cp $OPTIONS "$SCRIPTS/dot_files/dot_vim"/* "$TARGET"/; then
        echo "[ERROR] Failed to copy dot_vim files to $TARGET." >&2
        exit 1
    fi
}

# Install dot_vim configuration for nvim if exist
maybe_copy_to_nvim() {
    local nvim_config="$HOME/.config/nvim"
    if [ -d "$nvim_config" ]; then
        echo "[INFO] Detected $nvim_config. Copying dot_vim contents..."
        if ! cp $OPTIONS "$SCRIPTS/dot_files/dot_vim"/* "$nvim_config"/; then
            echo "[WARN] Failed to copy dot_vim files to $nvim_config." >&2
        else
            echo "[INFO] dot_vim files also copied to $nvim_config."
        fi
    else
        echo "[INFO] $nvim_config not found. Skipping NeoVim copy."
    fi
}

# Perform installation steps
install() {
    check_commands vim cp mkdir uname
    check_scripts

    if [ ! -d "$SCRIPTS/dot_files/dot_vim" ]; then
        echo "[ERROR] dot_vim source directory does not exist. Ensure that the SCRIPTS variable is correctly set." >&2
        exit 1
    fi

    echo "[INFO] Starting dot_vim installation..."
    setup_environment "$1"
    install_dotvim "$1"
    maybe_copy_to_nvim

    echo "[INFO] dot_vim configuration installed successfully at $TARGET."
}

# Remove dot_vim and optionally nvim configuration
uninstall() {
    check_commands rm

    echo "[INFO] Starting dot_vim uninstallation..."

    if [ -d "$HOME/.vim" ]; then
        echo "[INFO] Removing $HOME/.vim"
        if ! rm -rf "$HOME/.vim"; then
            echo "[ERROR] Failed to remove $HOME/.vim" >&2
            exit 1
        fi
    else
        echo "[INFO] $HOME/.vim does not exist. Skipping."
    fi

    if [ -d "$HOME/.config/nvim" ]; then
        echo "[INFO] Removing dot_vim files from $HOME/.config/nvim"
        if ! rm -f "$HOME/.config/nvim/"*; then
            echo "[WARN] Failed to clean files in $HOME/.config/nvim" >&2
        fi
    else
        echo "[INFO] $HOME/.config/nvim does not exist. Skipping."
    fi

    echo "[INFO] dot_vim uninstallation completed."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version)
            usage
            ;;
        -u|--uninstall)
            uninstall
            ;;
        ""|*)
            install
            ;;
    esac

    return 0
}

# Execute main function
main "$@"

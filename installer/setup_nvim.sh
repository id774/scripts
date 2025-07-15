#!/bin/sh

########################################################################
# setup_nvim.sh: Link NeoVim as Vim and migrate configuration
#
#  Description:
#  This script configures NeoVim to behave as a Vim replacement by:
#  - Creating the NeoVim configuration directory.
#  - Linking ~/.vimrc to ~/.config/nvim/init.vim.
#  - Creating a symlink to make 'vim' invoke 'nvim' via /usr/local/bin.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run this script to set up NeoVim as a drop-in Vim replacement:
#      ./setup_nvim.sh
#
#  Requirements:
#  - NeoVim must be installed and available in PATH.
#  - Requires sudo privileges to create a link in /usr/local/bin.
#
#  Version History:
#  v1.0 2025-07-15
#       Initial version.
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Function to create the NeoVim config directory if it does not exist
create_config_dir() {
    if [ ! -d "$HOME/.config/nvim" ]; then
        mkdir -p "$HOME/.config/nvim" || {
            echo "[ERROR] Failed to create ~/.config/nvim" >&2
            exit 1
        }
        echo "[INFO] Created directory ~/.config/nvim"
    else
        echo "[INFO] ~/.config/nvim already exists. Skipping."
    fi
}

# Function to create a symlink from ~/.vimrc to init.vim
link_vimrc() {
    target="$HOME/.config/nvim/init.vim"
    if [ ! -e "$target" ]; then
        ln -s "$HOME/.vimrc" "$target" || {
            echo "[ERROR] Failed to link ~/.vimrc to $target" >&2
            exit 1
        }
        echo "[INFO] Linked ~/.vimrc to $target"
    else
        echo "[INFO] $target already exists. Skipping."
    fi
}

# Function to locate the nvim executable path
find_nvim_path() {
    nvim_path=$(command -v nvim)
    echo "$nvim_path"
}

# Function to create a system-wide symlink from vim to nvim
create_vim_symlink() {
    nvim_path="$1"
    if [ ! -e /usr/local/bin/vim ]; then
        if ! sudo ln -s "$nvim_path" /usr/local/bin/vim; then
            echo "[ERROR] Failed to create symlink /usr/local/bin/vim" >&2
            exit 1
        fi
        echo "[INFO] Created symlink: /usr/local/bin/vim -> $nvim_path"
    else
        echo "[INFO] /usr/local/bin/vim already exists. Skipping."
    fi
}

# Execute main function
main() {
    check_commands sudo cp ln mkdir nvim
    check_sudo

    nvim_path=$(find_nvim_path)
    create_config_dir
    link_vimrc
    create_vim_symlink "$nvim_path"
    echo "[SUCCESS] NeoVim is now set up as a Vim replacement."
    return 0
}

main "$@"
exit $?

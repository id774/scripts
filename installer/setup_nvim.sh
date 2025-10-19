#!/bin/sh

########################################################################
# setup_nvim.sh: Configure NeoVim as Vim replacement and manage config
#
#  Description:
#  This script configures NeoVim to behave as a Vim replacement by:
#  - Creating the NeoVim configuration directory.
#  - Copying ~/.vimrc to ~/.config/nvim/init.vim.
#  - Creating a symlink to make 'vim' invoke 'nvim' via ~/.local/bin.
#
#  An uninstall option is also provided:
#  - Removes the entire ~/.config/nvim directory.
#  - Removes the symlink ~/.local/bin/vim if it is a link.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  To install:
#      ./setup_nvim.sh
#
#  To uninstall:
#      ./setup_nvim.sh --uninstall
#
#  Requirements:
#  - NeoVim must be installed and available in PATH.
#
#  Version History:
#  v1.1 2025-07-19
#       Replace symbolic link with copy of .vimrc and delete full config on uninstall.
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

# Create the NeoVim configuration directory if it does not exist
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

# Copy .vimrc to init.vim as the NeoVim configuration
copy_vimrc() {
    src="$HOME/.vimrc"
    dest="$HOME/.config/nvim/init.vim"

    if [ ! -f "$src" ]; then
        echo "[ERROR] Source file $src does not exist." >&2
        exit 1
    fi

    cp "$src" "$dest" || {
        echo "[ERROR] Failed to copy $src to $dest" >&2
        exit 1
    }
    echo "[INFO] Copied $src to $dest"
}

# Locate the nvim executable path
find_nvim_path() {
    command -v nvim
}

# Create a system-wide symlink from vim to nvim in ~/.local/bin
create_vim_symlink() {
    nvim_path="$1"
    target_dir="$HOME/.local/bin"

    if [ ! -d "$target_dir" ]; then
        mkdir -p "$target_dir" || {
            echo "[ERROR] Failed to create $target_dir" >&2
            exit 1
        }
        echo "[INFO] Created directory $target_dir"
    fi

    if [ ! -e "$target_dir/vim" ]; then
        ln -sf "$nvim_path" "$target_dir/vim" || {
            echo "[ERROR] Failed to create symlink $target_dir/vim" >&2
            exit 1
        }
        echo "[INFO] Created symlink: $target_dir/vim -> $nvim_path"
    else
        echo "[INFO] $target_dir/vim already exists. Skipping."
    fi
}

# Install NeoVim configuration and set up vim symlink
install() {
    check_commands mkdir cp ln nvim

    nvim_path=$(find_nvim_path)
    if [ -z "$nvim_path" ]; then
        echo "[ERROR] nvim not found in PATH." >&2
        exit 1
    fi

    create_config_dir
    copy_vimrc
    create_vim_symlink "$nvim_path"

    final_report
}

# Display final setup instructions and confirmation
final_report() {
    echo "[INFO] NeoVim is now set up as a Vim replacement."
    echo "[INFO] Add ~/.local/bin to the beginning of your PATH if not already present:"
    echo "       export PATH=~/.local/bin:\$PATH"
}

# Uninstall the NeoVim configuration and symlink created by this script
uninstall() {
    check_commands rm

    echo "[INFO] Uninstalling NeoVim configuration and vim symlink..."

    config_dir="$HOME/.config/nvim"
    if [ -d "$config_dir" ]; then
        rm -rf "$config_dir" && echo "[INFO] Removed directory: $config_dir"
    else
        echo "[INFO] $config_dir does not exist. Skipping."
    fi

    target_bin="$HOME/.local/bin/vim"
    if [ -L "$target_bin" ]; then
        rm "$target_bin" && echo "[INFO] Removed symlink: $target_bin"
    else
        echo "[INFO] $target_bin is not a symlink. Skipping."
    fi

    echo "[INFO] Uninstallation completed."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version)
            usage
            ;;
        -u|--uninstall|--uninstall)
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

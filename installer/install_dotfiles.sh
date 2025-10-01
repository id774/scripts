#!/bin/sh

########################################################################
# install_dotfiles.sh: Deploy and Configure User Dotfiles
#
#  Description:
#  This script automates the deployment and configuration of user dotfiles
#  across various environments, including macOS, Linux, and other Unix-like systems.
#  It ensures consistent user configurations by copying predefined dotfiles
#  to appropriate locations, managing permissions, and applying necessary settings.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_dotfiles.sh
#
#  Features:
#  - Deploys dotfiles such as .zshrc, .vimrc, .gitconfig, .emacs, etc.
#  - Ensures required directories exist and have correct permissions.
#  - Configures Emacs settings, including site-lisp and backup directories.
#  - Supports multiple users on Linux/macOS.
#  - Uses POSIX-compliant scripting practices for better compatibility.
#
#  Notes:
#  - Deploys dotfiles and configures user environments.
#  - Automatically detects and sets up configurations for multiple users.
#  - Run with appropriate permissions if modifying system-wide settings.
#
#  Version History:
#  v3.1 2025-09-05
#       Add xinputrc to dotfiles deployment
#  v3.0 2025-08-23
#       Add known_hosts deployment using a single shared ssh file copier.
#  v2.9 2025-07-21
#       Add logic to deploy user's ~/.ssh/config to target accounts if it exists.
#  v2.8 2025-07-12
#       Add emergencyadmin and munin users to dotfiles deployment targets.
#       Add ownership correction to deploy_dotfiles_to_mac.
#  v2.7 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v2.6 2025-04-25
#       Exit on failure during dotfile deployment and show success only if all steps succeed.
#  v2.5 2025-04-21
#       Added detailed [INFO] log messages to each step for improved visibility during execution.
#  v2.4 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v2.3 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v2.2 2025-03-13
#       Redirected error messages to stderr for better logging and debugging.
#       Optimize home directory ownership handling.
#  v2.1 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
#  v2.0 2025-03-04
#       Improved POSIX compliance by quoting variables and using safer constructs.
#       Ensured robust directory creation and permission handling.
#       Streamlined deployment logic for better maintainability.
#  [Further version history truncated for brevity]
#  v1.0 2010-03-02
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

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the path of your script collection." >&2
        exit 1
    fi
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

# Set up system-specific environment options
setup_environment() {
    case "$(uname -s)" in
        Darwin)
            OPTIONS="-v"
            ;;
        *)
            OPTIONS="-vd"
            ;;
    esac
}

# Create directories if they do not exist
mkdir_if_not_exist() {
    while [ "$#" -gt 0 ]
    do
        sudo test -d "$1" || sudo mkdir -p "$1"
        shift
    done
}

# Deploy user dotfiles such as .zshrc, .vimrc, etc.
deploy_dotfile() {
    for DOT_FILES in zshrc screenrc vimrc gvimrc gitconfig gitignore condarc gemrc Rprofile emacs xinputrc; do
        if test -d "$1"; then
            if ! sudo cp "$OPTIONS" "$SCRIPTS/dot_files/dot_$DOT_FILES" "$1/.$DOT_FILES"; then
                echo "[ERROR] Failed to copy dot_$DOT_FILES to $1." >&2
                exit 1
            fi
        fi
    done

    for DOT_FILES in pryrc; do
        if test -d "$1" && test -f "$1/.$DOT_FILES"; then
            if ! sudo rm -vf "$1/.$DOT_FILES"; then
                echo "[ERROR] Failed to remove $1/.$DOT_FILES." >&2
                exit 1
            fi
        fi
    done

    return 0
}

# Set up Emacs configuration directories and permissions
setup_dotemacs() {
    echo "[INFO] Setting up Emacs configuration under $1/.emacs.d."
    mkdir_if_not_exist \
      "$1/.emacs.d" \
      "$1/.emacs.d/site-lisp" \
      "$1/.emacs.d/anything" \
      "$1/.emacs.d/backups" \
      "$1/.emacs.d/tmp" \
      "$1/.emacs.d/tramp-auto-save" \
      "$1/.emacs.d/auto-save-list"
    sudo chmod 0750 "$1/.emacs.d" "$1/.emacs.d/site-lisp" "$1/.emacs.d/anything" "$1/.emacs.d/backups" "$1/.emacs.d/tmp" "$1/.emacs.d/tramp-auto-save" "$1/.emacs.d/auto-save-list"
    sudo touch "$1/.emacs.d/anything/anything-c-adaptive-history"
    test -d /usr/local/etc/emacs.d/elisp && \
      sudo ln -fs /usr/local/etc/emacs.d/elisp "$1/.emacs.d/elisp"
    test -L /usr/local/etc/emacs.d/elisp/elisp && \
      sudo rm -f /usr/local/etc/emacs.d/elisp/elisp
}

# Create user skeleton directories and set proper permissions
mkdir_skelton() {
    mkdir_if_not_exist \
      "$1/.tmp" "$1/.local" "$1/.config" "$1/tmp" "$1/mnt" "$1/local" "$1/var" "$1/etc" "$1/bin" "$1/arc"
    sudo chmod 0700 "$1/.tmp" "$1/.local" "$1/.config" "$1/tmp" "$1/mnt" "$1/var"
    sudo chmod 0750 "$1/local" "$1/etc" "$1/bin" "$1/arc"
    command -v emacs >/dev/null 2>&1 && setup_dotemacs "$1"
}

# Copy ~/.ssh/<name> to target user if exists
copy_ssh_file_if_exists() {
    dest_home="$1"
    dest_user="$2"
    name="$3"  # e.g., config or known_hosts

    src="$HOME/.ssh/$name"
    dest_ssh="$dest_home/.ssh"
    dest="$dest_ssh/$name"

    [ -f "$src" ] || return 0

    # Skip if source and destination are the same file
    if [ -e "$dest" ] && [ "$src" -ef "$dest" ]; then
        echo "[INFO] Skipping copy: source and destination are identical for $dest_user ($name)"
        return 0
    fi

    echo "[INFO] Copying .ssh/$name to $dest_user ($dest)"
    sudo mkdir -p "$dest_ssh"
    sudo cp "$OPTIONS" "$src" "$dest"
    sudo chmod 0700 "$dest_ssh"
    sudo chmod 0600 "$dest"
    sudo chown -R "$dest_user:$(id -gn "$dest_user")" "$dest_ssh"
}

# Deploy dotfiles and create necessary directories for a given user
deploy_dotfiles() {
    echo "[INFO] Copying dotfiles to $1."
    if ! deploy_dotfile "$1"; then
        echo "[ERROR] Failed to deploy dotfiles to $1." >&2
        exit 1
    fi

    mkdir_skelton "$1"
}

# Deploy dotfiles to a specific user and adjust ownership
deploy_dotfiles_to_others() {
    if [ -d "$1" ]; then
        deploy_dotfiles "$1"
        copy_ssh_file_if_exists "$1" "$2" config
        copy_ssh_file_if_exists "$1" "$2" known_hosts
        sudo chown -R "$2:$(id -gn "$2")" "$1"
    fi
}

# Deploy dotfiles to macOS users
deploy_dotfiles_to_mac() {
    while [ "$#" -gt 0 ]
    do
        user="$1"
        user_home="/Users/$user"

        if [ -d "$user_home" ] && id "$user" >/dev/null 2>&1; then
            echo "[INFO] Deploying dotfiles to macOS user: $user"
            deploy_dotfiles "$user_home"
            copy_ssh_file_if_exists "$user_home" "$user" config
            copy_ssh_file_if_exists "$user_home" "$user" known_hosts
            sudo chown "$user:$(id -gn "$user")" "$user_home"
            sudo find "$user_home" -maxdepth 1 -mindepth 1 -exec chown "$user:$(id -gn "$user")" {} +
        fi
        shift
    done
}

# Deploy dotfiles to Linux users
deploy_dotfiles_to_linux() {
    while [ "$#" -gt 0 ]
    do
        if [ -d "/home/$1" ]; then
            echo "[INFO] Deploying dotfiles to Linux user: $1"
            deploy_dotfiles "/home/$1"
            copy_ssh_file_if_exists "/home/$1" "$1" config
            copy_ssh_file_if_exists "/home/$1" "$1" known_hosts
            sudo chown "$1:$(id -gn "$1")" "/home/$1"
            sudo find "/home/$1" -maxdepth 1 -mindepth 1 -exec chown "$1:$(id -gn "$1")" {} +
        fi
        shift
    done
}

# Deploy dotfiles to multiple users across different OS environments
bulk_deploy() {
    test -d "/home" && test -d "/home/$USER" && sudo chmod 0750 /home/*
    test -d /home/opt && sudo chmod 0755 /home/opt

    # Harden local SSH source permissions if present
    if [ -d "$HOME/.ssh" ]; then
        chmod 0700 "$HOME/.ssh"
        [ -f "$HOME/.ssh/config" ] && chmod 0600 "$HOME/.ssh/config"
        [ -f "$HOME/.ssh/known_hosts" ] && chmod 0600 "$HOME/.ssh/known_hosts"
    fi

    deploy_dotfiles_to_linux \
      debian \
      ubuntu \
      redhat \
      centos \
      sl \
      admin \
      sysadmin \
      sysop \
      git \
      ec2-user \
      automatic \
      fluent \
      mongo \
      plagger \
      twitter \
      tiarra \
      testuser

    deploy_dotfiles_to_mac \
      adm \
      emergencyadmin \
      mac \
      apple \
      demo \
      work \
      testuser
    deploy_dotfiles_to_others /var/root root
    deploy_dotfiles_to_others /root root
    deploy_dotfiles_to_others /var/lib/munin munin
    deploy_dotfiles_to_others /var/lib/postgresql postgres
    deploy_dotfiles_to_others /var/lib/pgsql postgres
    deploy_dotfiles_to_others /usr/lib/oracle/xe oracle
    deploy_dotfiles_to_others /export/home/solaris solaris
    deploy_dotfiles_to_others /var/lib/jenkins jenkins
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    # Check if required commands are available and executable
    check_commands sudo cp mkdir chmod chown id rm ln find zsh uname touch realpath
    check_scripts
    setup_environment "$1"
    check_sudo

    echo "[INFO] Starting dotfiles deployment process."
    bulk_deploy

    rm -f "$HOME/.zshrc.zwc"
    cd || exit 1
    zsh -c 'zcompile "$HOME/.zshrc"'

    test -f "$HOME/etc/config.local/dot_gitconfig" && \
      cp "$OPTIONS" "$HOME/etc/config.local/dot_gitconfig" "$HOME/.gitconfig"

    test -f "$HOME/.viminfo" && sudo chown "$USER" "$HOME/.viminfo"

    echo "[INFO] dot_files configuration installed successfully."
    return 0
}

# Execute main function
main "$@"
exit $?

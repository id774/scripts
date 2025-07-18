#!/bin/sh

########################################################################
# debian_env.sh: Environment Setup for Debian
#
#  Description:
#  This script configures the base environment for Debian-based systems.
#  It ensures that apt-get is available, performs system updates,
#  and configures essential administrative groups and filesystem tuning.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#  Run the script directly:
#      ./debian_env.sh
#
#  Ensure that the required setup scripts exist in the designated directory.
#
#  Notes:
#  - The script is designed for Debian-based systems (Debian, Ubuntu, etc.).
#  - Internet connectivity is required for package updates.
#  - Review and modify configurations as needed before execution.
#
#  Error Conditions:
#  - If apt-get is unavailable, the script exits with an error.
#  - If required commands are missing, execution is halted.
#  - Errors from underlying scripts should be resolved based on their output.
#
#  Version History:
#  v1.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v1.2 2025-04-13
#       Unify log level formatting using [INFO], [WARN], and [ERROR] tags.
#  v1.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v1.0 2025-03-13
#       Enhanced documentation and comments for better maintainability.
#       Ensured strict POSIX compliance.
#       Redirected error messages to stderr for better logging and debugging.
#  [Further version history truncated for brevity]
#  v0.1 2011-09-28
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

# Check if the system supports apt-get
check_environment() {
    if ! command -v apt-get >/dev/null 2>&1; then
        echo "[ERROR] apt-get is not available on this system. This script requires a Debian-based environment." >&2
        exit 1
    fi
}

# Verify script environment
setup_environment() {
    SCRIPTS="$HOME/scripts"
    if [ ! -d "$SCRIPTS" ]; then
        echo "[ERROR] Directory '$SCRIPTS' does not exist. Please create it or specify the correct path." >&2
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

# Check if the user has sudo privileges
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Set locale ja_JP.UTF-8
set_locale_jp() {
    # Install `locales` package if `/etc/locale.gen` does not exist
    if [ ! -f /etc/locale.gen ]; then
        sudo apt-get install -y locales
        echo "ja_JP.UTF-8 UTF-8" | sudo tee -a /etc/locale.gen
    fi

    # Append `ja_JP.UTF-8 UTF-8` to `/etc/locale.gen` if not already present
    if ! grep -q '^ja_JP.UTF-8' /etc/locale.gen; then
        echo "ja_JP.UTF-8 UTF-8" | sudo tee -a /etc/locale.gen
    fi

    # Generate locale if `ja_JP.UTF-8` is not available
    if ! locale -a | grep -q '^ja_JP\.UTF-8$' || [ "$(locale | grep '^LANG=')" != "LANG=ja_JP.UTF-8" ]; then
        sudo locale-gen
    fi

    # Update `LANG` to `ja_JP.UTF-8` if not set
    if ! locale | grep -q '^LANG=ja_JP.UTF-8$'; then
        sudo update-locale LANG=ja_JP.UTF-8
        export LANG=ja_JP.UTF-8
    fi
}

# Perform system update and upgrade
apt_upgrade() {
    sudo apt-get update &&
    sudo apt-get -y upgrade &&
    sudo apt-get autoclean &&
    sudo apt-get -y autoremove
}

# Create administrative groups
create_admin_group() {
    sudo groupadd -f admin
    sudo groupadd -f wheel
}

# Set up filesystem tuning
setup_tune2fs() {
    "$SCRIPTS/installer/setup_tune2fs.sh"
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version) usage ;;
    esac

    check_environment
    setup_environment
    check_commands sudo tee locale locale-gen update-locale
    check_sudo

    set_locale_jp
    apt_upgrade
    create_admin_group
    setup_tune2fs

    echo "[INFO] All Debian environment setup completed."
    return 0
}

# Execute main function
main "$@"
exit $?

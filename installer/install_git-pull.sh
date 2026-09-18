#!/bin/sh

########################################################################
# install_git-pull.sh: Deploy git-pull Cron Job
#
#  Description:
#  This is an installer that deploys the cron definition for the scheduled
#  execution of git-all-pull.sh. It:
#  - Deploys the tracked cron/etc/cron.d/git-pull to /etc/cron.d/git-pull.
#  - Does not overwrite an existing deployed cron configuration on reinstall.
#  - Removes only the deployed cron definition on uninstall.
#
#  Author: id774 (More info: https://id774.net)
#  Source Code: https://github.com/id774/scripts
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_git-pull.sh
#      ./install_git-pull.sh --uninstall
#
#  Requirements:
#  - Linux system.
#  - The SCRIPTS environment variable must point to the scripts repository
#    root when running install mode.
#  - The invoking user must have sudo privileges.
#
#  Exit Status:
#      0: The selected workflow completed, or usage/help/version was displayed.
#      1: System, environment, sudo, or critical install/uninstall failure.
#      126: A required command exists but is not executable.
#      127: A required command is not found.
#
#  Notes:
#  - If /etc/cron.d/git-pull already exists, it is not overwritten.
#  - After installation, review /etc/cron.d/git-pull and edit the schedule,
#    user, MAILTO, SCRIPTS path, and options to match the host.
#  - The default sample runs daily at 08:00 as user 'debian', with
#    MAILTO=debian, SCRIPTS=/home/debian/scripts, and options
#    '--github-only --www-only'.
#  - Standard output is discarded; standard error is left for cron's mail
#    delivery.
#  - The default cron job intentionally has no dedicated job log. It runs as
#    the repository-owning non-root user, while /var/log/sysadmin is reserved
#    for administrator-readable logs written by system-managed processes.
#  - Do not create a user-writable git-pull log under /var/log/sysadmin or grant
#    the cron user write access there. Discarding stdout and leaving stderr for
#    cron mail is intentional.
#  - '--uninstall' removes only /etc/cron.d/git-pull.
#
#  Version History:
#  v1.0 2026-09-17
#       Initial release.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    check_commands awk
    awk '
        BEGIN { in_header = 0 }
        /^#+$/ && length($0) >= 10 { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if the system is Linux
check_system() {
    check_commands uname
    if [ "$(uname -s 2>/dev/null)" != "Linux" ]; then
        echo "[ERROR] This script is intended for Linux systems only." >&2
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

# Check if SCRIPTS variable is set
check_scripts() {
    if [ -z "$SCRIPTS" ]; then
        echo "[ERROR] SCRIPTS environment variable is not set." >&2
        echo "Please set the SCRIPTS variable to the scripts repository root." >&2
        exit 1
    fi
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    check_commands sudo
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Deploy the git-all-pull cron job
install() {
    check_system
    check_commands cp chmod chown
    check_scripts
    check_sudo

    if [ ! -e /etc/cron.d/git-pull ]; then
        echo "[INFO] Installing cron job to /etc/cron.d/git-pull"
        if ! sudo cp "$SCRIPTS/cron/etc/cron.d/git-pull" /etc/cron.d/git-pull; then
            echo "[ERROR] Failed to copy cron configuration." >&2
            exit 1
        fi
    else
        echo "[INFO] Cron job already exists: /etc/cron.d/git-pull"
        echo "[INFO] Skipping creation to preserve existing configuration."
    fi

    if ! sudo chown root:adm /etc/cron.d/git-pull; then
        echo "[ERROR] Failed to set ownership on /etc/cron.d/git-pull." >&2
        exit 1
    fi
    if ! sudo chmod 0640 /etc/cron.d/git-pull; then
        echo "[ERROR] Failed to set permissions on /etc/cron.d/git-pull." >&2
        exit 1
    fi

    echo "[INFO] Installation of git-all-pull cron setup completed successfully."
    echo "# Notes: Please review and edit '/etc/cron.d/git-pull'"
    echo "# to ensure the schedule, user, MAILTO, SCRIPTS path, and options meet your operational requirements."
}

# Remove the deployed git-all-pull cron job
uninstall() {
    check_system
    check_commands rm
    check_sudo

    echo "[INFO] Uninstalling git-all-pull cron setup..."

    if [ -e /etc/cron.d/git-pull ]; then
        echo "[INFO] Removing /etc/cron.d/git-pull"
        if ! sudo rm -f /etc/cron.d/git-pull; then
            echo "[ERROR] Failed to remove /etc/cron.d/git-pull" >&2
            exit 1
        fi
    else
        echo "[INFO] /etc/cron.d/git-pull not found. Skipping."
    fi

    echo "[INFO] Uninstallation completed."
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
        "")
            install
            ;;
        *)
            usage
            ;;
    esac

    return 0
}

# Execute main function
main "$@"
